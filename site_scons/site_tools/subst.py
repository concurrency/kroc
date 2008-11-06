import string
from SCons.Builder import Builder
from SCons.Action import Action

class AtSubstTemplate(string.Template):
    pattern = r"""
      @(?:
        (?P<escaped>@)   |
        (?P<named>.*?)@  |
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
    contents.write(t.safe_substitute(getattr(env, 'SUBST', dict())))
    contents.close()
    return 0

    
subst_builder = Builder(action = Action(subst_build_function, 
                                        "Substituting in $TARGET from $SOURCE"),
                        src_suffix = '.in',
                        single_source=True)

def generate(env, **kw):
    env['BUILDERS']['Subst'] = subst_builder

def exists(env):
    return 1
