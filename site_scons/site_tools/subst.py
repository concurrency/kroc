import string
from SCons.Builder import Builder
from SCons.Action import Action

class AtSubstTemplate(string.Template):
    delimiter = '@'
    pattern = r"""
      @(?:
        (?P<escaped>@)   |
        (?P<named>.*?)@  |
        (?P<braced>.*?)@ |
        (?P<invalid>))
        """

def subst_build_function(target, source, env):
    source = str(source[0])
    target = str(target[0])

    infile   = open(source, 'r')
    contents = infile.read()
    infile.close()

    t = AtSubstTemplate(contents)
    contents = open(target, 'w+')
    contents.write(t.safe_substitute(env.get('SUBST', dict())))
    contents.close()
    return 0

    
subst_builder = Builder(action = Action(subst_build_function, 
                                        "Substituting in $TARGET from $SOURCE"),
                        src_suffix = '.in',
                        single_source=True)

def generate(env, **kw):
    env['BUILDERS']['Substitute'] = subst_builder

def exists(env):
    return 1


if __name__ == '__main__':
    text = ''' @test@
    @testing@
    @notsupplied@
    @with_underscore@
    two ats: @@
    just one: @
    '''
    print text
    print '-' * 60
    print AtSubstTemplate(text).safe_substitute(dict(
        test='TEST',
        testing='TESTING',
        with_underscore='WITH_UNDERSCORE'))
