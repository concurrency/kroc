import string
from SCons.Builder import Builder
from SCons.Action import Action
from SCons import Node, Util

class AtSubstTemplate(string.Template):
    """Substitutes things surrounded by @s"""
    delimiter = '@'
    pattern = r"""
      @(?:
        (?P<escaped>@)   |
        (?P<named>.*?)@  |
        (?P<braced>.*?)@ |
        (?P<invalid>))
        """

def expand_dict(d, env):
    """Takes a dictionary and returns a copy of it with values expanded from
    the envronment, env"""
    d = d.copy() # copy it
    for (k,v) in d.items():
        if callable(v):
            d[k] = env.subst(v())
        elif Util.is_String(v):
            d[k]=env.subst(v)        
    return d

def subst_build_function(target, source, env):
    source = str(source[0])
    target = str(target[0])

    infile   = open(source, 'r')
    contents = infile.read()
    infile.close()

    t = AtSubstTemplate(contents)
    contents = open(target, 'w+')
    contents.write(t.safe_substitute(expand_dict(env.get('SUBST', dict()), env)))
    contents.close()
    return 0

def subst_emitter(target, source, env):
    """Make the target depend on the values stored in the substitution dict so
    that things will get rebuilt when the values are changed"""
    env.Depends(target, Node.Python.Value(expand_dict(env.get('SUBST', dict()), env)))
    return (target, source)

def generate(env, **kw):
    subst_builder = Builder(action = Action(subst_build_function, 
                                            "Substituting in $TARGET from $SOURCE"),
                            emitter=subst_emitter,                
                            src_suffix = '.in',
                            single_source=True)
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
