
import unittest
try:
    from collections import OrderedDict
except ImportError:
    from utils import OrderedDict

import re

class NoSingleValueFoundException(Exception):
    pass

def read_namelist_file(filename):
    return Namelist(open(filename, 'r').read())


class AttributeMapper():
    """
    Simple mapper to access dictionary items as attributes
    """

    def __init__(self, obj):
        self.__dict__['data'] = obj

    def __getattr__(self, attr):
        if attr in self.data:
            found_attr = self.data[attr]
            if isinstance(found_attr, dict):
                return AttributeMapper(found_attr)
            else:
                return found_attr
        else:
            raise AttributeError

    def __setattr__(self, attr, value):
        if attr in self.data:
            self.data[attr] = value
        else:
            raise NotImplementedError

    def __dir__(self):
        return self.data.keys()

class CaseInsensitiveDict(OrderedDict):
    """
    This is an ordered dictionary which ignores the letter case of the
    keys given (if the respective key is a string).

    Many thanks to user m000 who answered
    https://stackoverflow.com/questions/2082152/case-insensitive-dictionary
    so helpfully.
    """
    
    @classmethod
    def _k(cls, key):
        return key.lower() if isinstance(key, str) else key

    def __init__(self, *args, **kwargs):
        super(CaseInsensitiveDict, self).__init__(*args, **kwargs)
        self._convert_keys()
    def __getitem__(self, key):
        return super(CaseInsensitiveDict, self).__getitem__(self.__class__._k(key))
    def __setitem__(self, key, value):
        super(CaseInsensitiveDict, self).__setitem__(self.__class__._k(key), value)
    def __delitem__(self, key):
        return super(CaseInsensitiveDict, self).__delitem__(self.__class__._k(key))
    def __contains__(self, key):
        return super(CaseInsensitiveDict, self).__contains__(self.__class__._k(key))
    def has_key(self, key):
        return super(CaseInsensitiveDict, self).has_key(self.__class__._k(key))
    def pop(self, key, *args, **kwargs):
        return super(CaseInsensitiveDict, self).pop(self.__class__._k(key), *args, **kwargs)
    def get(self, key, *args, **kwargs):
        return super(CaseInsensitiveDict, self).get(self.__class__._k(key), *args, **kwargs)
    def setdefault(self, key, *args, **kwargs):
        return super(CaseInsensitiveDict, self).setdefault(self.__class__._k(key), *args, **kwargs)
    def update(self, E={}, **F):
        super(CaseInsensitiveDict, self).update(self.__class__(E))
        super(CaseInsensitiveDict, self).update(self.__class__(**F))
    def _convert_keys(self):
        for k in list(self.keys()):
            v = super(CaseInsensitiveDict, self).pop(k)
            self.__setitem__(k, v)

class Namelist():
    """
    Parses namelist files in Fortran 90 format, recognised groups are
    available through 'groups' attribute.
    """

    def __init__(self, input_str):
        self.groups = CaseInsensitiveDict()

        namelist_start_line_re = re.compile(r'^\s*&(\w+)\s*$')
        namelist_end_line_re = re.compile(r'^\s*/\s*$')

        # a pattern matching an array of stuff
        a_number = r'[0-9\.\+\-eE]+'
        #array_re = re.compile(r'(?:\s*([^,](?:\'[^\']*\')(?:\(\s*'+a_number+'\s*,\s*'+a_number+'\s*\))*)\s,)*')

        # a comma-separated list, of elements which either do not
        # contain a comma, or may contain commas inside strings, or
        # may contain commas inside paretheses.
        # At the end of the line, the comma is optional.
        # FIXME deal with abbrev. lists!
        # FIXME strings containing parentheses will cause problems with this expression
        array_re = re.compile(r"([\w.+-]+|\s*\'[^\']*\'\s*|\s[(][^),]+,[^),]+[)]\s*)(?:,|,?\s*$)")
        string_re = re.compile(r"\'\s*\w[^']*\'")
        self._complex_re = re.compile(r'^\((\d+.?\d*),(\d+.?\d*)\)$')

        # a pattern to match the non-comment part of a line. This
        # should be able to deal with ! signs inside strings.
        comment_re = re.compile(r"((?:[^\'!]*(?:\'[^\']*\'))*)!.*")

        # match notation for Fortran logicals in namelist files:
        self.logical_true_re = re.compile(r"[^tTfF\']*[tT].*")
        self.logical_false_re = re.compile(r"[^tTfF\']*[fF].*")

        # match abbreviated lists of identical items, like
        # 509*-1.0000000000000000
        # 253*0
        # NEO_EQUIL_PARSE_SP_SEQ=          1,          2, 2*3          , 28*-1
        # 60*"          "
        self.abbrev_list_re = re.compile(r"([0-9]+)\*(.+)")

        # commas at the end of lines seem to be optional
        keyval_line_re = re.compile(r"\s*(\w+)\s*=\s*(.+),?")

        group = CaseInsensitiveDict()
        current_group = None
        for line in input_str.split('\n'):
            # remove comments
            line_without_comment = comment_re.sub(r"\1",line)
            # remove whitespaces
            line_without_comment = line_without_comment.strip()
            if len(line_without_comment) == 0:
                continue

          
            m = namelist_start_line_re.match(line_without_comment)
            if(m):
                if(current_group is None):
                    current_group = m.group(1)
                    group = CaseInsensitiveDict()
                    continue
                else:
                    raise SyntaxError('Namelist %s starts, but namelist %s is not yet complete.' % (m.group(1),current_group))

            m = namelist_end_line_re.match(line_without_comment)
            if(m):
                if(current_group is not None):
                    current_group = None
                    continue
                else:
                    raise SyntaxError('End of namelist encountered, but there is no corresponding open namelist.')
                    
            # other lines: key = value, or a continuation line
            m = keyval_line_re.match(line_without_comment)
            if(m):
                if(current_group is not None):
                    variable_name = m.group(1)
                    variable_value = m.group(2)

                    print(variable_name)
                    print(variable_value)
                    # parse the array with self-crafted regex
                    parsed_list = array_re.findall(variable_value)
                    print(parsed_list)
                    parsed_list = [self._parse_value(elem) for elem in parsed_list]

                    # if it wasnt for special notations like .false. or 60*'' , one could
                    # simply use a parser from the python standard library for the right
                    # hand side as a whole:
                    #parsed_value = ast.literal_eval(variable_value)
                    try:
                        if(len(parsed_list) == 1):
                            group[variable_name] = parsed_list[0]
                        else:
                            group[variable_name] = parsed_list
                    except TypeError:
                        group[variable_name] = parsed_list
                    
                else:
                    raise SyntaxError('Key %s encountered, but there is no enclosing namelist' % variable_name)

            self.groups[current_group] = group

            #self._check_lists()

    def _parse_value(self, variable_value_str):
        """
        Tries to parse a single value, raises a SyntaxError if not successful.
        """
        import ast
        try:
            parsed_value = ast.literal_eval(variable_value_str.strip())
        except (ValueError, SyntaxError):
            print(variable_value_str.strip())

            abbrev_list_match = self.abbrev_list_re.match(variable_value_str)
            if(abbrev_list_match):
                parsed_value = int(abbrev_list_match.group(1)) * [self._parse_value(abbrev_list_match.group(2))]
            elif(self.logical_true_re.match(variable_value_str)):
                parsed_value = True
            elif(self.logical_false_re.match(variable_value_str)):
                parsed_value = False
            else:
                raise SyntaxError('Right hand side expression could not be parsed. The string is: %s' % (variable_value_str))

                #FIXME distinguish complex scalar and a list of 2 reals
                #FIXME rstrip strings, because this is what fortran does
        try:
            if(len(parsed_value) == 1):
                # one gets a list of length 1 if the line ends with a
                # comma, because (4,) for python is a tuple with one
                # element, and (4) is just the scalar 4.
                return parsed_value[0]
            else:
                return parsed_value
        except TypeError:
            return parsed_value
            

    def _check_lists(self):
        for group in self.groups.values():
            for variable_name, variable_values in group.items():
                if isinstance(variable_values, dict):
                    if '_is_list' in variable_values and variable_values['_is_list']:
                        variable_data = variable_values
                        del(variable_data['_is_list'])

                        num_entries = len(variable_data.keys())
                        variable_list = [None]*num_entries

                        for i, value in variable_data.items():
                            if i >= num_entries:
                                raise Exception("The variable '%s' has an array index assignment that is inconsistent with the number of list values" % variable)
                            else:
                                variable_list[i] = value

                        group[variable_name] = variable_list

    def dump(self, array_inline=True):
        lines = []
        for group_name, group_variables in self.groups.items():
            lines.append("&%s" % group_name)
            for variable_name, variable_value in group_variables.items():
                if isinstance(variable_value, list):
                    if array_inline:
                        lines.append("%s= %s" % (variable_name, " ".join([self._format_value(v) for v in variable_value])))
                    else:
                        for n, v in enumerate(variable_value):
                            lines.append("%s(%d)=%s" % (variable_name, n+1, self._format_value(v)))
                else:
                    lines.append("%s=%s" % (variable_name, self._format_value(variable_value)))
            lines.append("/")

        return "\n".join(lines)

    def _format_value(self, value):
        if isinstance(value, bool):
            return value and '.true.' or '.false.'
        elif isinstance(value, int):
            return "%d" % value
        elif isinstance(value, float):
            return "%f" % value
        elif isinstance(value, str):
            return "'%s'" % value
        elif isinstance(value, complex):
            return "(%s,%s)" % (self._format_value(value.real), self._format_value(value.imag))
        else:
            raise Exception("Variable type not understood: %s" % type(value))

    @property
    def data(self):
        return AttributeMapper(self.groups)

class ParsingTests(unittest.TestCase):
    def test_single_value(self):
        input_str = """
        &CCFMSIM_SETUP
        CCFMrad=800.0
        /
        """
        namelist = Namelist(input_str)

        expected_output = {'CCFMSIM_SETUP': { 'CCFMrad': 800. }}

        self.assertEqual(namelist.groups, expected_output)

    def test_multigroup(self):
        input_str = """
        &CCFMSIM_SETUP
        CCFMrad=800.0
        /
        &GROUP2
        R=500.0
        /
        """
        namelist = Namelist(input_str)

        expected_output = {'CCFMSIM_SETUP': { 'CCFMrad': 800. },
                           'GROUP2': { 'R': 500. }}

        self.assertEqual(namelist.groups, expected_output)

    def test_comment(self):
        input_str = """
        ! Interesting comment at the start
        &CCFMSIM_SETUP
        CCFMrad=800.0
        ! And a comment some where in the middle
        /
        &GROUP2
        R=500.0
        /
        """
        namelist = Namelist(input_str)

        expected_output = {'CCFMSIM_SETUP': { 'CCFMrad': 800. },
                           'GROUP2': { 'R': 500. }}

        self.assertEqual(namelist.groups, expected_output)

    def test_array(self):
        input_str = """
        &CCFMSIM_SETUP
        ntrac_picture=4
        var_trac_picture(1)='watcnew'
        des_trac_picture(1)='cloud_water'
        var_trac_picture(2)='watpnew'
        des_trac_picture(2)='rain'
        var_trac_picture(3)='icecnew'
        des_trac_picture(3)='cloud_ice'
        var_trac_picture(4)='granew'
        des_trac_picture(4)='graupel'
        /
        """
        namelist = Namelist(input_str)

        expected_output = {
            'CCFMSIM_SETUP': {
                'ntrac_picture': 4,
                'var_trac_picture': [
                    'watcnew',
                    'watpnew',
                    'icecnew',
                    'granew',
                ],
                'des_trac_picture': [
                    'cloud_water',
                    'rain',
                    'cloud_ice',
                    'graupel',
                ],
            },
        }

        self.assertEqual(dict(namelist.groups), expected_output)

    def test_boolean_sciformat(self):
        input_str = """
        &ATHAM_SETUP

        nz      =300
        zstart  =0.
        ztotal  =15000.
        dzzoom  =50.
        kcenter =20
        nztrans =0
        nztrans_boundary =6

        cpumax  =9.e6

        no_uwind=.false.
        no_vwind=.true.
        /
        """
        namelist = Namelist(input_str)

        expected_output = {
            'ATHAM_SETUP': {
                'nz': 300,
                'zstart': 0.,
                'ztotal': 15000.,
                'dzzoom': 50.,
                'kcenter': 20,
                'nztrans': 0,
                'nztrans_boundary': 6,
                'cpumax': 9.e6,
                'no_uwind': False,
                'no_vwind': True,
            }
        }

        self.assertEqual(dict(namelist.groups), expected_output)

    def test_comment_with_forwardslash(self):
        input_str = """
        ! Interesting comment at the start
        &CCFMSIM_SETUP
        CCFMrad=800.0
        ! And a comment some where in the middle/halfway !
        var2=40
        /
        &GROUP2
        R=500.0
        /
        """
        namelist = Namelist(input_str)

        expected_output = {'CCFMSIM_SETUP': { 'CCFMrad': 800., 'var2': 40 },
                           'GROUP2': { 'R': 500. }}

        self.assertEqual(namelist.groups, expected_output)

    def test_inline_array(self):
        input_str = """
        ! can have blank lines and comments in the namelist input file
        ! place these comments between NAMELISTs

        !
        ! not every compiler supports comments within the namelist
        !   in particular vastf90/g77 does not
        !
        ! some will skip NAMELISTs not directly referenced in read
        !&BOGUS rko=1 /
        !
        &TTDATA
        TTREAL =  1.,
        TTINTEGER = 2,
        TTCOMPLEX = (3.,4.),
        TTCHAR = 'namelist',
        TTBOOL = T/
        &AADATA
        AAREAL =  1.  1.  2.  3.,
        AAINTEGER = 2 2 3 4,
        AACOMPLEX = (3.,4.) (3.,4.) (5.,6.) (7.,7.),
        AACHAR = 'namelist' 'namelist' 'array' ' the lot',
        AABOOL = T T F F/
        &XXDATA
        XXREAL =  1.,
        XXINTEGER = 2,
        XXCOMPLEX = (3.,4.)/! can have blank lines and comments in the namelist input file
        """

        expected_output = {
            'TTDATA': {
                'TTREAL': 1.,
                'TTINTEGER': 2,
                'TTCOMPLEX': 3. + 4.j,
                'TTCHAR': 'namelist',
                'TTBOOL': True,
            },
            'AADATA': {
                'AAREAL': [1., 1., 2., 3.,],
                'AAINTEGER': [2, 2, 3, 4],
                'AACOMPLEX': [3.+4.j, 3.+4.j, 5.+6.j, 7.+7.j],
                'AACHAR': ['namelist', 'namelist', 'array', ' the lot'],
                'AABOOL': [True, True, False, False],
            },
            'XXDATA': {
                'XXREAL': 1.,
                'XXINTEGER': 2.,
                'XXCOMPLEX': 3.+4.j,
            },
        }

        namelist = Namelist(input_str)

        self.assertEqual(dict(namelist.groups), expected_output)

class ParsingTests(unittest.TestCase):
    def test_single_value(self):
        input_str = """&CCFMSIM_SETUP
CCFMrad=800.000000
/"""
        namelist = Namelist(input_str)

        self.assertEqual(namelist.dump(), input_str)

    def test_multigroup(self):
        input_str = """&CCFMSIM_SETUP
CCFMrad=800.000000
/
&GROUP2
R=500.000000
/"""
        namelist = Namelist(input_str)

        self.assertEqual(namelist.dump(), input_str)


    def test_array(self):
        input_str = """&CCFMSIM_SETUP
var_trac_picture(1)='watcnew'
var_trac_picture(2)='watpnew'
var_trac_picture(3)='icecnew'
var_trac_picture(4)='granew'
des_trac_picture(1)='cloud_water'
des_trac_picture(2)='rain'
des_trac_picture(3)='cloud_ice'
des_trac_picture(4)='graupel'
/"""
        namelist = Namelist(input_str)

        self.assertEqual(namelist.dump(array_inline=False), input_str)

    def test_inline_array(self):
        input_str = """&AADATA
AACOMPLEX= (3.000000,4.000000) (3.000000,4.000000) (5.000000,6.000000) (7.000000,7.000000)
/"""

        namelist = Namelist(input_str)

        print(input_str)
        print(namelist.dump())

        self.assertEqual(namelist.dump(), input_str)

if __name__=='__main__':
    unittest.main()
