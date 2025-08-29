# -*- coding: utf-8 -*-
"""
Created on Sun Jan 26 22:22:20 2025

@author: Radha Sharma
"""

https://www.w3resource.com/python-exercises/
https://www.w3resource.com/sql-exercises/adventureworks/adventureworks-exercises.php


1. Python Output

# Python is a case sensitive language
print('Hello World')
     
Hello World

print('salman khan')
     
salman khan

print(salman khan)
     
  File "<ipython-input-3-0713073d8d88>", line 1
    print(salman khan)
                    ^
SyntaxError: invalid syntax

print(7)
     
7

print(7.7)
     
7.7

print(True)
     
True

print('Hello',1,4.5,True)
     
Hello 1 4.5 True

print('Hello',1,4.5,True,sep='/')
     
Hello/1/4.5/True

print('hello')
print('world')
     
hello
world

print('hello',end='-')
print('world')
     
hello-world
2. Data Types

# Integer
print(8)
# 1*10^308
print(1e309)
     
8
inf

# Decimal/Float
print(8.55)
print(1.7e309)
     
8.55
inf

# Boolean
print(True)
print(False)
     
True
False

# Text/String
print('Hello World')
     
Hello World

# complex
print(5+6j)
     
(5+6j)

# List-> C-> Array
print([1,2,3,4,5])
     
[1, 2, 3, 4, 5]

# Tuple
print((1,2,3,4,5))
     
(1, 2, 3, 4, 5)

# Sets
print({1,2,3,4,5})
     
{1, 2, 3, 4, 5}

# Dictionary
print({'name':'Nitish','gender':'Male','weight':70})
     
{'name': 'Nitish', 'gender': 'Male', 'weight': 70}

# type
type([1,2,3])
     
list
3. Variables

# Static Vs Dynamic Typing
# Static Vs Dynamic Binding
# stylish declaration techniques
     

# C/C++
name = 'nitish'
print(name)

a = 5
b = 6

print(a + b)
     
nitish
11

# Dynamic Typing
a = 5
# Static Typing
int a = 5
     

# Dynamic Binding
a = 5
print(a)
a = 'nitish'
print(a)

# Static Binding
int a = 5

     
5
nitish

a = 1
b = 2
c = 3
print(a,b,c)
     
1 2 3

a,b,c = 1,2,3
print(a,b,c)
     
1 2 3

a=b=c= 5
print(a,b,c)
     
5 5 5
Comments

# this is a comment
# second line
a = 4
b = 6 # like this
# second comment
print(a+b)
     
10
4. Keywords & Identifiers

# Keywords
     

# Identifiers
# You can't start with a digit
name1 = 'Nitish'
print(name1)
# You can use special chars -> _
_ = 'ntiish'
print(_)
# identiers can not be keyword
     
Nitish
ntiish
Temp Heading
5. User Input

# Static Vs Dynamic
input('Enter Email')
     
Enter Emailnitish@gmail.com
'nitish@gmail.com'

# take input from users and store them in a variable
fnum = int(input('enter first number'))
snum = int(input('enter second number'))
#print(type(fnum),type(snum))
# add the 2 variables
result = fnum + snum
# print the result
print(result)
print(type(fnum))
     
enter first number56
enter second number67
123
<class 'int'>
6. Type Conversion

# Implicit Vs Explicit
print(5+5.6)
print(type(5),type(5.6))

print(4 + '4')
     
10.6
<class 'int'> <class 'float'>
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-57-72e5c45cdb6f> in <module>
      3 print(type(5),type(5.6))
      4 
----> 5 print(4 + '4')

TypeError: unsupported operand type(s) for +: 'int' and 'str'

# Explicit
# str -> int
#int(4+5j)

# int to str
str(5)

# float
float(4)
     
4.0
7. Literals

a = 0b1010 #Binary Literals
b = 100 #Decimal Literal
c = 0o310 #Octal Literal
d = 0x12c #Hexadecimal Literal

#Float Literal
float_1 = 10.5
float_2 = 1.5e2 # 1.5 * 10^2
float_3 = 1.5e-3 # 1.5 * 10^-3

#Complex Literal
x = 3.14j

print(a, b, c, d)
print(float_1, float_2,float_3)
print(x, x.imag, x.real)
     

# binary
x = 3.14j
print(x.imag)
     
3.14

string = 'This is Python'
strings = "This is Python"
char = "C"
multiline_str = """This is a multiline string with more than one line code."""
unicode = u"\U0001f600\U0001F606\U0001F923"
raw_str = r"raw \n string"

print(string)
print(strings)
print(char)
print(multiline_str)
print(unicode)
print(raw_str)
     
This is Python
This is Python
C
This is a multiline string with more than one line code.
😀😆🤣
raw \n string

a = True + 4
b = False + 10

print("a:", a)
print("b:", b)
     
a: 5
b: 10

k = None
a = 5
b = 6
print('Program exe')
     
Program exe
8. Operators

# Arithmetic
# Relational
# Logical
# Bitwise
# Assignment
# Membership
     
9. If-Else


################################################################################  



What is an Iteration
Iteration is a general term for taking each item of something, one after another. Any time you use a loop, explicit or implicit, to go over a group of items, that is iteration.

# Example
num = [1,2,3]

for i in num:
    print(i)
1
2
3
What is Iterator
An Iterator is an object that allows the programmer to traverse through a sequence of data without having to store the entire data in the memory

# Example
L = [x for x in range(1,10000)]

#for i in L:
    #print(i*2)
    
import sys

print(sys.getsizeof(L)/64)

x = range(1,10000000000)

#for i in x:
    #print(i*2)
    
print(sys.getsizeof(x)/64)
1369.0
0.75
What is Iterable
Iterable is an object, which one can iterate over

It generates an Iterator when passed to iter() method.

# Example

L = [1,2,3]
type(L)


# L is an iterable
type(iter(L))

# iter(L) --> iterator
list_iterator
Point to remember
Every Iterator is also and Iterable
Not all Iterables are Iterators
Trick
Every Iterable has an iter function
Every Iterator has both iter function as well as a next function
a = 2
a

#for i in a:
    #print(i)
    
dir(a)
['__abs__',
 '__add__',
 '__and__',
 '__bool__',
 '__ceil__',
 '__class__',
 '__delattr__',
 '__dir__',
 '__divmod__',
 '__doc__',
 '__eq__',
 '__float__',
 '__floor__',
 '__floordiv__',
 '__format__',
 '__ge__',
 '__getattribute__',
 '__getnewargs__',
 '__gt__',
 '__hash__',
 '__index__',
 '__init__',
 '__init_subclass__',
 '__int__',
 '__invert__',
 '__le__',
 '__lshift__',
 '__lt__',
 '__mod__',
 '__mul__',
 '__ne__',
 '__neg__',
 '__new__',
 '__or__',
 '__pos__',
 '__pow__',
 '__radd__',
 '__rand__',
 '__rdivmod__',
 '__reduce__',
 '__reduce_ex__',
 '__repr__',
 '__rfloordiv__',
 '__rlshift__',
 '__rmod__',
 '__rmul__',
 '__ror__',
 '__round__',
 '__rpow__',
 '__rrshift__',
 '__rshift__',
 '__rsub__',
 '__rtruediv__',
 '__rxor__',
 '__setattr__',
 '__sizeof__',
 '__str__',
 '__sub__',
 '__subclasshook__',
 '__truediv__',
 '__trunc__',
 '__xor__',
 'as_integer_ratio',
 'bit_length',
 'conjugate',
 'denominator',
 'from_bytes',
 'imag',
 'numerator',
 'real',
 'to_bytes']
T = {1:2,3:4}
dir(T)
['__class__',
 '__contains__',
 '__delattr__',
 '__delitem__',
 '__dir__',
 '__doc__',
 '__eq__',
 '__format__',
 '__ge__',
 '__getattribute__',
 '__getitem__',
 '__gt__',
 '__hash__',
 '__init__',
 '__init_subclass__',
 '__iter__',
 '__le__',
 '__len__',
 '__lt__',
 '__ne__',
 '__new__',
 '__reduce__',
 '__reduce_ex__',
 '__repr__',
 '__reversed__',
 '__setattr__',
 '__setitem__',
 '__sizeof__',
 '__str__',
 '__subclasshook__',
 'clear',
 'copy',
 'fromkeys',
 'get',
 'items',
 'keys',
 'pop',
 'popitem',
 'setdefault',
 'update',
 'values']
L = [1,2,3]

# L is not an iterator
iter_L = iter(L)

# iterator = iter(iterable)

# iter_L is an iterator
['__class__',
 '__delattr__',
 '__dir__',
 '__doc__',
 '__eq__',
 '__format__',
 '__ge__',
 '__getattribute__',
 '__gt__',
 '__hash__',
 '__init__',
 '__init_subclass__',
 '__iter__',
 '__le__',
 '__length_hint__',
 '__lt__',
 '__ne__',
 '__new__',
 '__next__',
 '__reduce__',
 '__reduce_ex__',
 '__repr__',
 '__setattr__',
 '__setstate__',
 '__sizeof__',
 '__str__',
 '__subclasshook__']
Understanding how for loop works
num = [1,2,3]

for i in num:
    print(i)
1
2
3
num = [1,2,3]

# fetch the iterator
iter_num = iter(num)

# step2 --> next
next(iter_num)
next(iter_num)
next(iter_num)
next(iter_num)
---------------------------------------------------------------------------
StopIteration                             Traceback (most recent call last)
<ipython-input-47-0793ebf651f8> in <module>
      8 next(iter_num)
      9 next(iter_num)
---> 10 next(iter_num)

StopIteration: 
Making our own for loop
def mera_khudka_for_loop(iterable):
    
    iterator = iter(iterable)
    
    while True:
        
        try:
            print(next(iterator))
        except StopIteration:
            break           
a = [1,2,3]
b = range(1,11)
c = (1,2,3)
d = {1,2,3}
e = {0:1,1:1}

mera_khudka_for_loop(e)
0
1
A confusing point
num = [1,2,3]
iter_obj = iter(num)

print(id(iter_obj),'Address of iterator 1')

iter_obj2 = iter(iter_obj)
print(id(iter_obj2),'Address of iterator 2')
2280889893936 Address of iterator 1
2280889893936 Address of iterator 2
Let's create our own range() function
class mera_range:
    
    def __init__(self,start,end):
        self.start = start
        self.end = end
        
    def __iter__(self):
        return mera_range_iterator(self)
class mera_range_iterator:
    
    def __init__(self,iterable_obj):
        self.iterable = iterable_obj
    
    def __iter__(self):
        return self
    
    def __next__(self):
        
        if self.iterable.start >= self.iterable.end:
            raise StopIteration
            
        current = self.iterable.start
        self.iterable.start+=1
        return current
x = mera_range(1,11)
type(x)
__main__.mera_range
iter(x)
<__main__.mera_range_iterator at 0x2130fd362b0>
 


##################################################################################   




What is a Generator
Python generators are a simple way of creating iterators.

# iterable
class mera_range:
    
    def __init__(self,start,end):
        self.start = start
        self.end = end
        
    def __iter__(self):
        return mera_iterator(self)
    

# iterator
class mera_iterator:
    
    def __init__(self,iterable_obj):
        self.iterable = iterable_obj
    
    def __iter__(self):
        return self
    
    def __next__(self):
        
        if self.iterable.start >= self.iterable.end:
            raise StopIteration
        
        current = self.iterable.start
        self.iterable.start+=1
        return current
The Why
L = [x for x in range(100000)]

#for i in L:
    #print(i**2)
    
import sys
sys.getsizeof(L)

x = range(10000000)

#for i in x:
    #print(i**2)
sys.getsizeof(x)
48
A Simple Example
def gen_demo():
    
    yield "first statement"
    yield "second statement"
    yield "third statement"
gen = gen_demo()

for i in gen:
    print(i)
first statement
second statement
third statement
Python Tutor Demo (yield vs return)
 
Example 2
def square(num):
    for i in range(1,num+1):
        yield i**2
gen = square(10)

print(next(gen))
print(next(gen))
print(next(gen))

for i in gen:
    print(i)
1
4
9
16
25
36
49
64
81
100
Range Function using Generator
def mera_range(start,end):
    
    for i in range(start,end):
        yield i
for i in mera_range(15,26):
    print(i)
15
16
17
18
19
20
21
22
23
24
25
Generator Expression
# list comprehension
L = [i**2 for i in range(1,101)]
gen = (i**2 for i in range(1,101))

for i in gen:
    print(i)
1
4
9
16
25
36
49
64
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729
784
841
900
961
1024
1089
1156
1225
1296
1369
1444
1521
1600
1681
1764
1849
1936
2025
2116
2209
2304
2401
2500
2601
2704
2809
2916
3025
3136
3249
3364
3481
3600
3721
3844
3969
4096
4225
4356
4489
4624
4761
4900
5041
5184
5329
5476
5625
5776
5929
6084
6241
6400
6561
6724
6889
7056
7225
7396
7569
7744
7921
8100
8281
8464
8649
8836
9025
9216
9409
9604
9801
10000
Practical Example
import os
import cv2

def image_data_reader(folder_path):

    for file in os.listdir(folder_path):
        f_array = cv2.imread(os.path.join(folder_path,file))
        yield f_array
    
gen = image_data_reader('C:/Users/91842/emotion-detector/train/Sad')

next(gen)
next(gen)

next(gen)
next(gen)
array([[[ 38,  38,  38],
        [ 26,  26,  26],
        [ 23,  23,  23],
        ...,
        [198, 198, 198],
        [196, 196, 196],
        [167, 167, 167]],

       [[ 32,  32,  32],
        [ 25,  25,  25],
        [ 26,  26,  26],
        ...,
        [194, 194, 194],
        [204, 204, 204],
        [181, 181, 181]],

       [[ 44,  44,  44],
        [ 42,  42,  42],
        [ 38,  38,  38],
        ...,
        [156, 156, 156],
        [214, 214, 214],
        [199, 199, 199]],

       ...,

       [[150, 150, 150],
        [165, 165, 165],
        [186, 186, 186],
        ...,
        [229, 229, 229],
        [226, 226, 226],
        [239, 239, 239]],

       [[145, 145, 145],
        [156, 156, 156],
        [180, 180, 180],
        ...,
        [227, 227, 227],
        [228, 228, 228],
        [221, 221, 221]],

       [[144, 144, 144],
        [150, 150, 150],
        [172, 172, 172],
        ...,
        [211, 211, 211],
        [189, 189, 189],
        [217, 217, 217]]], dtype=uint8)
Benefits of using a Generator
1. Ease of Implementation
class mera_range:
    
    def __init__(self,start,end):
        self.start = start
        self.end = end
        
    def __iter__(self):
        return mera_iterator(self)
# iterator
class mera_iterator:
    
    def __init__(self,iterable_obj):
        self.iterable = iterable_obj
    
    def __iter__(self):
        return self
    
    def __next__(self):
        
        if self.iterable.start >= self.iterable.end:
            raise StopIteration
        
        current = self.iterable.start
        self.iterable.start+=1
        return current
def mera_range(start,end):
    
    for i in range(start,end):
        yield i
2. Memory Efficient
L = [x for x in range(100000)]
gen = (x for x in range(100000))

import sys

print('Size of L in memory',sys.getsizeof(L))
print('Size of gen in memory',sys.getsizeof(gen))
Size of L in memory 824456
Size of gen in memory 112
3. Representing Infinite Streams
def all_even():
    n = 0
    while True:
        yield n
        n += 2
even_num_gen = all_even()
next(even_num_gen)
next(even_num_gen)
2
4. Chaining Generators
def fibonacci_numbers(nums):
    x, y = 0, 1
    for _ in range(nums):
        x, y = y, x+y
        yield x

def square(nums):
    for num in nums:
        yield num**2

print(sum(square(fibonacci_numbers(10))))
4895
 





###################################################################################





Operators in Python
Arithmetic Operators
Relational Operators
Logical Operators
Bitwise Operators
Assignment Operators
Membership Operators

# Arithmetric Operators
print(5+6)

print(5-6)

print(5*6)

print(5/2)

print(5//2)

print(5%2)

print(5**2)
     
11
-1
30
2.5
2
1
25

# Relational Operators
print(4>5)

print(4<5)

print(4>=4)

print(4<=4)

print(4==4)

print(4!=4)
     
False
True
True
True
True
False

# Logical Operators
print(1 and 0)

print(1 or 0)

print(not 1)
     
0
1
False

# Bitwise Operators

# bitwise and
print(2 & 3)

# bitwise or
print(2 | 3)

# bitwise xor
print(2 ^ 3)

print(~3)

print(4 >> 2)

print(5 << 2)
     
2
3
1
-4
1
20

# Assignment Operators

# =
# a = 2

a = 2

# a = a % 2
a %= 2

# a++ ++a

print(a)
     
4

# Membership Operators

# in/not in

print('D' not in 'Delhi')

print(1 in [2,3,4,5,6])
     
False
False

# Program - Find the sum of a 3 digit number entered by the user

number = int(input('Enter a 3 digit number'))

# 345%10 -> 5
a = number%10

number = number//10

# 34%10 -> 4
b = number % 10

number = number//10
# 3 % 10 -> 3
c = number % 10

print(a + b + c)
     
Enter a 3 digit number666
18
If-else in Python

# login program and indentation
# email -> nitish.campusx@gmail.com
# password -> 1234

email = input('enter email')
password = input('enter password')

if email == 'nitish.campusx@gmail.com' and password == '1234':
  print('Welcome')
elif email == 'nitish.campusx@gmail.com' and password != '1234':
  # tell the user
  print('Incorrect password')
  password = input('enter password again')
  if password == '1234':
    print('Welcome,finally!')
  else:
    print('beta tumse na ho paayega!')
else:
  print('Not correct')
     
enter emailsrhreh
enter passworderhetjh
Not correct

# if-else examples
# 1. Find the min of 3 given numbers
# 2. Menu Driven Program
     
first num4
second num1
third num10
smallest is 1

# menu driven calculator
menu = input("""
Hi! how can I help you.
1. Enter 1 for pin change
2. Enter 2 for balance check
3. Enter 3 for withdrawl
4. Enter 4 for exit
""")

if menu == '1':
  print('pin change')
elif menu == '2':
  print('balance')
else:
  print('exit')
     
Hi! how can I help you.
1. Enter 1 for pin change
2. Enter 2 for balance check
3. Enter 3 for withdrawl
4. Enter 4 for exit
2
balance
Modules in Python
math
keywords
random
datetime

# math
import math

math.sqrt(196)
     
14.0

# keyword
import keyword
print(keyword.kwlist)
     
['False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await', 'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is', 'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return', 'try', 'while', 'with', 'yield']

# random
import random
print(random.randint(1,100))
     
88

# datetime
import datetime
print(datetime.datetime.now())
     
2022-11-08 15:50:21.228643

help('modules')
     
Please wait a moment while I gather a list of all available modules...

/usr/local/lib/python3.7/dist-packages/caffe2/proto/__init__.py:17: UserWarning: Caffe2 support is not enabled in this PyTorch build. Please enable Caffe2 by building PyTorch from source with `BUILD_CAFFE2=1` flag.
/usr/local/lib/python3.7/dist-packages/caffe2/proto/__init__.py:17: UserWarning: Caffe2 support is not enabled in this PyTorch build. Please enable Caffe2 by building PyTorch from source with `BUILD_CAFFE2=1` flag.
/usr/local/lib/python3.7/dist-packages/caffe2/python/__init__.py:9: UserWarning: Caffe2 support is not enabled in this PyTorch build. Please enable Caffe2 by building PyTorch from source with `BUILD_CAFFE2=1` flag.
Cython              collections         kaggle              requests_oauthlib
IPython             colorcet            kanren              resampy
OpenGL              colorlover          kapre               resource
PIL                 colorsys            keras               rlcompleter
ScreenResolution    community           keras_preprocessing rmagic
__future__          compileall          keyword             rpy2
_abc                concurrent          kiwisolver          rsa
_ast                confection          korean_lunar_calendar runpy
_asyncio            configparser        langcodes           samples
_bisect             cons                lib2to3             sched
_blake2             contextlib          libfuturize         scipy
_bootlocale         contextlib2         libpasteurize       scs
_bz2                contextvars         librosa             seaborn
_cffi_backend       convertdate         lightgbm            secrets
_codecs             copy                linecache           select
_codecs_cn          copyreg             llvmlite            selectors
_codecs_hk          crashtest           lmdb                send2trash
_codecs_iso2022     crcmod              locale              setuptools
_codecs_jp          crypt               locket              setuptools_git
_codecs_kr          csimdjson           logging             shapely
_codecs_tw          csv                 lsb_release         shelve
_collections        ctypes              lunarcalendar       shlex
_collections_abc    cufflinks           lxml                shutil
_compat_pickle      curses              lzma                signal
_compression        cv2                 macpath             simdjson
_contextvars        cvxopt              mailbox             site
_crypt              cvxpy               mailcap             sitecustomize
_csv                cycler              markdown            six
_ctypes             cymem               markupsafe          skimage
_ctypes_test        cython              marshal             sklearn
_curses             cythonmagic         marshmallow         sklearn_pandas
_curses_panel       daft                math                slugify
_cvxcore            dask                matplotlib          smart_open
_datetime           dataclasses         matplotlib_venn     smtpd
_dbm                datascience         mimetypes           smtplib
_decimal            datetime            missingno           sndhdr
_distutils_hack     dateutil            mistune             snowballstemmer
_dlib_pybind11      dbm                 mizani              socket
_dummy_thread       dbus                mlxtend             socketserver
_ecos               debugpy             mmap                socks
_elementtree        decimal             modulefinder        sockshandler
_functools          decorator           more_itertools      softwareproperties
_hashlib            defusedxml          moviepy             sortedcontainers
_heapq              descartes           mpmath              soundfile
_imp                difflib             msgpack             spacy
_io                 dill                multidict           spacy_legacy
_json               dis                 multipledispatch    spacy_loggers
_locale             distributed         multiprocessing     sphinx
_lsprof             distutils           multitasking        spwd
_lzma               dlib                murmurhash          sql
_markupbase         dns                 music21             sqlalchemy
_md5                docs                natsort             sqlite3
_multibytecodec     doctest             nbconvert           sqlparse
_multiprocessing    docutils            nbformat            sre_compile
_opcode             dopamine            netCDF4             sre_constants
_operator           dot_parser          netrc               sre_parse
_osx_support        dummy_threading     networkx            srsly
_pickle             easydict            nibabel             ssl
_plotly_future_     ecos                nis                 stan
_plotly_utils       editdistance        nisext              stat
_posixsubprocess    ee                  nltk                statistics
_py_abc             email               nntplib             statsmodels
_pydecimal          en_core_web_sm      notebook            storemagic
_pyio               encodings           ntpath              string
_pyrsistent_version entrypoints         nturl2path          stringprep
_pytest             enum                numba               struct
_queue              ephem               numbergen           subprocess
_random             erfa                numbers             sunau
_remote_module_non_scriptable errno               numexpr             symbol
_rinterface_cffi_abi et_xmlfile          numpy               sympy
_rinterface_cffi_api etils               oauth2client        sympyprinting
_scs_direct         etuples             oauthlib            symtable
_scs_indirect       fa2                 ogr                 sys
_sha1               fastai              okgrade             sysconfig
_sha256             fastcore            opcode              syslog
_sha3               fastdownload        openpyxl            tables
_sha512             fastdtw             operator            tabnanny
_signal             fastjsonschema      opt_einsum          tabulate
_sitebuiltins       fastprogress        optparse            tarfile
_socket             fastrlock           os                  tblib
_soundfile          faulthandler        osgeo               telnetlib
_sqlite3            fcntl               osqp                tempfile
_sre                feather             osqppurepy          tenacity
_ssl                filecmp             osr                 tensorboard
_stat               fileinput           ossaudiodev         tensorboard_data_server
_string             filelock            packaging           tensorboard_plugin_wit
_strptime           firebase_admin      palettable          tensorflow
_struct             fix_yahoo_finance   pandas              tensorflow_datasets
_symtable           flask               pandas_datareader   tensorflow_estimator
_sysconfigdata_m_linux_x86_64-linux-gnu flatbuffers         pandas_gbq          tensorflow_gcs_config
_sysconfigdata_m_x86_64-linux-gnu fnmatch             pandas_profiling    tensorflow_hub
_testbuffer         folium              pandocfilters       tensorflow_io_gcs_filesystem
_testcapi           formatter           panel               tensorflow_metadata
_testimportmultiple fractions           param               tensorflow_probability
_testmultiphase     frozenlist          parser              termcolor
_thread             fsspec              parso               terminado
_threading_local    ftplib              partd               termios
_tkinter            functools           past                test
_tracemalloc        future              pasta               testpath
_warnings           gast                pastel              tests
_weakref            gc                  pathlib             text_unidecode
_weakrefset         gdal                pathy               textblob
_xxtestfuzz         gdalconst           patsy               textwrap
_yaml               gdalnumeric         pdb                 thinc
abc                 gdown               pep517              this
absl                genericpath         pexpect             threading
aeppl               gensim              pickle              threadpoolctl
aesara              geographiclib       pickleshare         tifffile
aifc                geopy               pickletools         time
aiohttp             getopt              pip                 timeit
aiosignal           getpass             pipes               tkinter
alabaster           gettext             piptools            tlz
albumentations      gi                  pkg_resources       token
altair              gin                 pkgutil             tokenize
antigravity         glob                platform            toml
apiclient           glob2               plistlib            tomli
appdirs             gnm                 plotly              toolz
apt                 google_auth_httplib2 plotlywidget        torch
apt_inst            google_auth_oauthlib plotnine            torchaudio
apt_pkg             google_drive_downloader pluggy              torchgen
aptsources          googleapiclient     pooch               torchsummary
argparse            googlesearch        poplib              torchtext
array               graphviz            portpicker          torchvision
arviz               greenlet            posix               tornado
ast                 gridfs              posixpath           tqdm
astor               grp                 pprint              trace
astropy             grpc                prefetch_generator  traceback
astunparse          gspread             preshed             tracemalloc
async_timeout       gspread_dataframe   prettytable         traitlets
asynchat            gym                 profile             tree
asyncio             gym_notices         progressbar         tty
asyncore            gzip                promise             turtle
asynctest           h5py                prompt_toolkit      tweepy
atari_py            hashlib             prophet             typeguard
atexit              heapdict            pstats              typer
atomicwrites        heapq               psutil              types
attr                hijri_converter     psycopg2            typing
attrs               hmac                pty                 typing_extensions
audioop             holidays            ptyprocess          tzlocal
audioread           holoviews           pvectorc            unicodedata
autograd            html                pwd                 unification
autoreload          html5lib            py                  unittest
babel               http                py_compile          uritemplate
backcall            httpimport          pyarrow             urllib
base64              httplib2            pyasn1              urllib3
bdb                 httplib2shim        pyasn1_modules      uu
bin                 httpstan            pyclbr              uuid
binascii            humanize            pycocotools         vega_datasets
binhex              hyperopt            pycparser           venv
bisect              idna                pyct                vis
bleach              imageio             pydantic            warnings
blis                imagesize           pydata_google_auth  wasabi
bokeh               imaplib             pydoc               wave
boost               imblearn            pydoc_data          wcwidth
branca              imgaug              pydot               weakref
bs4                 imghdr              pydot_ng            webargs
bson                imp                 pydotplus           webbrowser
builtins            importlib           pydrive             webencodings
bz2                 importlib_metadata  pyemd               werkzeug
cProfile            importlib_resources pyexpat             wheel
cachecontrol        imutils             pygments            widgetsnbextension
cached_property     inflect             pygtkcompat         wordcloud
cachetools          inspect             pylab               wrapt
caffe2              intervaltree        pylev               wsgiref
calendar            io                  pymc                xarray
catalogue           ipaddress           pymeeus             xarray_einstats
certifi             ipykernel           pymongo             xdrlib
cffi                ipykernel_launcher  pymystem3           xgboost
cftime              ipython_genutils    pyparsing           xkit
cgi                 ipywidgets          pyrsistent          xlrd
cgitb               isympy              pysndfile           xlwt
chardet             itertools           pytest              xml
charset_normalizer  itsdangerous        python_utils        xmlrpc
chunk               jax                 pytz                xxlimited
clang               jaxlib              pyviz_comms         xxsubtype
click               jieba               pywt                yaml
client              jinja2              pyximport           yarl
clikit              joblib              qdldl               yellowbrick
cloudpickle         jpeg4py             qudida              zict
cmake               json                queue               zipapp
cmath               jsonschema          quopri              zipfile
cmd                 jupyter             random              zipimport
cmdstanpy           jupyter_client      re                  zipp
code                jupyter_console     readline            zlib
codecs              jupyter_core        regex               zmq
codeop              jupyterlab_plotly   reprlib             
colab               jupyterlab_widgets  requests            

Enter any module name to get more help.  Or, type "modules spam" to search
for modules whose name or summary contain the string "spam".

Loops in Python
Need for loops
While Loop
For Loop

# While loop example -> program to print the table
# Program -> Sum of all digits of a given number
# Program -> keep accepting numbers from users till he/she enters a 0 and then find the avg
     

number = int(input('enter the number'))

i = 1

while i<11:
  print(number,'*',i,'=',number * i)
  i += 1
     
enter the number12
12 * 1 = 12
12 * 2 = 24
12 * 3 = 36
12 * 4 = 48
12 * 5 = 60
12 * 6 = 72
12 * 7 = 84
12 * 8 = 96
12 * 9 = 108
12 * 10 = 120

# while loop with else

x = 1

while x < 3:
  print(x)
  x += 1

else:
  print('limit crossed')
     
1
2
limit crossed

# Guessing game

# generate a random integer between 1 and 100
import random
jackpot = random.randint(1,100)

guess = int(input('guess karo'))
counter = 1
while guess != jackpot:
  if guess < jackpot:
    print('galat!guess higher')
  else:
    print('galat!guess lower')

  guess = int(input('guess karo'))
  counter += 1

else:
  print('correct guess')
  print('attempts',counter)



     
guess karo7
galat!guess higher
guess karo50
galat!guess lower
guess karo30
galat!guess higher
guess karo40
galat!guess lower
guess karo35
galat!guess lower
guess karo32
galat!guess higher
guess karo33
correct guess
attempts 7

# For loop demo

for i in {1,2,3,4,5}:
  print(i)
     
1
2
3
4
5

# For loop examples
     
Program - The current population of a town is 10000. The population of the town is increasing at the rate of 10% per year. You have to write a program to find out the population at the end of each of the last 10 years.

curr_pop = 10000

for i in range(10,0,-1):
  print(i,curr_pop)
  curr_pop = curr_pop - 0.1*curr_pop

     
10 10000
9 9000.0
8 8100.0
7 7290.0
6 6561.0
5 5904.9
4 5314.41
3 4782.969
2 4304.6721
1 3874.20489
Sequence sum
1/1! + 2/2! + 3/3! + ...


# code here
     

# For loop vs While loops (When to use what?)
     
Nested Loops

# Examples
     

# Program - Unique combination of 1,2,3,4
# Program - Pattern 1 and 2
     
Pattern 1
***
****
***



     
Pattern 2
1
121
12321
1234321



     
Loop Control Statement
Break
Continue
Pass

# Break demo
     

# Break example (Linear Search) -> Prime number in a given range
     

# Continue demo
     

# Continue Example (Ecommerce)
     

# Pass demo





################################################################################


Program - The current population of a town is 10000. The population of the town is increasing at the rate of 10% per year. You have to write a program to find out the population at the end of each of the last 10 years.

# Code here
curr_pop = 10000

for i in range(10,0,-1):
  print(i,curr_pop)
  curr_pop = curr_pop/1.1

     
10 10000
9 9090.90909090909
8 8264.462809917353
7 7513.148009015775
6 6830.134553650703
5 6209.213230591548
4 5644.739300537771
3 5131.5811823070635
2 4665.07380209733
1 4240.976183724845
Sequence sum
1/1! + 2/2! + 3/3! + ...


# Code here

n = int(input('enter n'))

result = 0
fact = 1

for i in range(1,n+1):
  fact = fact * i
  result = result + i/fact

print(result)



     
enter n2
2.0
Nested Loops

# Examples -> unique pairs

for i in range(1,5):
  for j in range(1,5):
    print(i,j)
     
1 1
1 2
1 3
1 4
2 1
2 2
2 3
2 4
3 1
3 2
3 3
3 4
4 1
4 2
4 3
4 4
Pattern 1
***
****
***


# code here

rows = int(input('enter number of rows'))

for i in range(1,rows+1):
  for j in range(1,i+1):
    print('*',end='')
  print()

     
enter number of rows10
*
**
***
****
*****
******
*******
********
*********
**********
Pattern 2
1
121
12321
1234321


# Code here
rows = int(input('enter number of rows'))

for i in range(1,rows+1):
  for j in range(1,i+1):
    print(j,end='')
  for k in range(i-1,0,-1):
    print(k,end='')

  print()

     
enter number of rows4
1
121
12321
1234321
Loop Control Statement
Break
Continue
Pass

for i in range(1,10):
  if i == 5:
    break
  print(i)
     
1
2
3
4

lower = int(input('enter lower range'))
upper = int(input('enter upper range'))

for i in range(lower,upper+1):
  for j in range(2,i):
    if i%j == 0:
      break
  else:
    print(i)
     
enter lower range10
enter upper range100
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97

# Continue
for i in range(1,10):
  if i == 5:
    continue
  print(i)
     
1
2
3
4
6
7
8
9

for i in range(1,10):
  pass

     
Strings are sequence of Characters

In Python specifically, strings are a sequence of Unicode Characters

Creating Strings
Accessing Strings
Adding Chars to Strings
Editing Strings
Deleting Strings
Operations on Strings
String Functions
Creating Stings

s = 'hello'
s = "hello"
# multiline strings
s = '''hello'''
s = """hello"""
s = str('hello')
print(s)
     
hello

"it's raining outside"
     
"it's raining outside"
Accessing Substrings from a String

# Positive Indexing
s = 'hello world'
print(s[41])
     
---------------------------------------------------------------------------
IndexError                                Traceback (most recent call last)
<ipython-input-61-633ba99ed6e5> in <module>
      1 # Positive Indexing
      2 s = 'hello world'
----> 3 print(s[41])

IndexError: string index out of range

# Negative Indexing
s = 'hello world'
print(s[-3])
     
r

# Slicing
s = 'hello world'
print(s[6:0:-2])
     
wol

print(s[::-1])
     
dlrow olleh

s = 'hello world'
print(s[-1:-6:-1])
     
dlrow
Editing and Deleting in Strings

s = 'hello world'
s[0] = 'H'

# Python strings are immutable
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-80-0c8a824e3b73> in <module>
      1 s = 'hello world'
----> 2 s[0] = 'H'

TypeError: 'str' object does not support item assignment

s = 'hello world'
del s
print(s)
     
---------------------------------------------------------------------------
NameError                                 Traceback (most recent call last)
<ipython-input-81-9ae37fbf1c6c> in <module>
      1 s = 'hello world'
      2 del s
----> 3 print(s)

NameError: name 's' is not defined

s = 'hello world'
del s[-1:-5:2]
print(s)
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-82-d0d823eafb6b> in <module>
      1 s = 'hello world'
----> 2 del s[-1:-5:2]
      3 print(s)

TypeError: 'str' object does not support item deletion
Operations on Strings
Arithmetic Operations
Relational Operations
Logical Operations
Loops on Strings
Membership Operations

print('delhi' + ' ' + 'mumbai')
     
delhi mumbai

print('delhi'*5)
     
delhidelhidelhidelhidelhi

print("*"*50)
     
**************************************************

'delhi' != 'delhi'
     
False

'mumbai' > 'pune'
# lexiographically
     
False

'Pune' > 'pune'
     
False

'hello' and 'world'
     
'world'

'hello' or 'world'
     
'hello'

'' and 'world'
     
''

'' or 'world'
     
'world'

'hello' or 'world'
     
'hello'

'hello' and 'world'
     
'world'

not 'hello'
     
False

for i in 'hello':
  print(i)
     
h
e
l
l
o

for i in 'delhi':
  print('pune')
     
pune
pune
pune
pune
pune

'D' in 'delhi'
     
False


     


     


     
Common Functions
len
max
min
sorted

len('hello world')
     
11

max('hello world')
     
'w'

min('hello world')
     
' '

sorted('hello world',reverse=True)
     
['w', 'r', 'o', 'o', 'l', 'l', 'l', 'h', 'e', 'd', ' ']


     
Capitalize/Title/Upper/Lower/Swapcase

s = 'hello world'
print(s.capitalize())
print(s)
     
Hello world
hello world

s.title()
     
'Hello World'

s.upper()
     
'HELLO WORLD'

'Hello Wolrd'.lower()
     
'hello wolrd'

'HeLlO WorLD'.swapcase()
     
'hElLo wORld'
Count/Find/Index

'my name is nitish'.count('i')
     
3

'my name is nitish'.find('x')
     
-1

'my name is nitish'.index('x')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-121-12e2ad5b75e9> in <module>
----> 1 'my name is nitish'.index('x')

ValueError: substring not found


     
endswith/startswith

'my name is nitish'.endswith('sho')
     
False

'my name is nitish'.startswith('1my')
     
False
format

name = 'nitish'
gender = 'male'

'Hi my name is {1} and I am a {0}'.format(gender,name)
     
'Hi my name is nitish and I am a male'
isalnum/ isalpha/ isdigit/ isidentifier

'nitish1234%'.isalnum()
     
False

'nitish'.isalpha()
     
True

'123abc'.isdigit()
     
False

'first-name'.isidentifier()
     
False


     
Split/Join

'hi my name is nitish'.split()
     
['hi', 'my', 'name', 'is', 'nitish']

" ".join(['hi', 'my', 'name', 'is', 'nitish'])
     
'hi my name is nitish'
Replace

'hi my name is nitish'.replace('nitisrgewrhgh','campusx')
     
'hi my name is nitish'
Strip

'nitish                           '.strip()
     
'nitish'
Example Programs

# Find the length of a given string without using the len() function

s = input('enter the string')

counter = 0

for i in s:
  counter += 1

print('length of string is',counter)
     
enter the stringnitish
length of string is 6

# Extract username from a given email.
# Eg if the email is nitish24singh@gmail.com
# then the username should be nitish24singh

s = input('enter the email')

pos = s.index('@')
print(s[0:pos])


     
enter the emailsupport@campusx.in
support

# Count the frequency of a particular character in a provided string.
# Eg 'hello how are you' is the string, the frequency of h in this string is 2.

s = input('enter the email')
term = input('what would like to search for')

counter = 0
for i in s:
  if i == term:
    counter += 1

print('frequency',counter)

     
enter the emailhi how are you
what would like to search foro
frequency 2

# Write a program which can remove a particular character from a string.
s = input('enter the string')
term = input('what would like to remove')

result = ''

for i in s:
  if i != term:
    result = result + i

print(result)
     
enter the stringnitish
what would like to removei
ntsh

# Write a program that can check whether a given string is palindrome or not.
# abba
# malayalam

s = input('enter the string')
flag = True
for i in range(0,len(s)//2):
  if s[i] != s[len(s) - i -1]:
    flag = False
    print('Not a Palindrome')
    break

if flag:
  print('Palindrome')


     
enter the stringpython
Not a Palindrome

# Write a program to count the number of words in a string without split()

s = input('enter the string')
L = []
temp = ''
for i in s:

  if i != ' ':
    temp = temp + i
  else:
    L.append(temp)
    temp = ''

L.append(temp)
print(L)


     
enter the stringhi how are you
['hi', 'how', 'are', 'you']

# Write a python program to convert a string to title case without using the title()
s = input('enter the string')

L = []
for i in s.split():
  L.append(i[0].upper() + i[1:].lower())

print(" ".join(L))
     
enter the stringhi my namE iS NitiSh
Hi My Name Is Nitish

# Write a program that can convert an integer to string.

number = int(input('enter the number'))

digits = '0123456789'
result = ''
while number != 0:
  result = digits[number % 10] + result
  number = number//10

print(result)
print(type(result))
     
enter the number345
345
<class 'str'>


     
###############################################################################



1. Lists
What are Lists?
Lists Vs Arrays
Characterstics of a List
How to create a list
Access items from a List
Editing items in a List
Deleting items from a List
Operations on Lists
Functions on Lists
What are Lists
List is a data type where you can store multiple items under 1 name. More technically, lists act like dynamic arrays which means you can add more items on the fly.

image.png

Why Lists are required in programming?
Array Vs Lists
Fixed Vs Dynamic Size
Convenience -> Hetrogeneous
Speed of Execution
Memory

L = [1,2,3]

print(id(L))
print(id(L[0]))
print(id(L[1]))
print(id(L[2]))
print(id(1))
print(id(2))
print(id(3))
     
140163201133376
11126688
11126720
11126752
11126688
11126720
11126752
How lists are stored in memory
Characterstics of a List
Ordered
Changeble/Mutable
Hetrogeneous
Can have duplicates
are dynamic
can be nested
items can be accessed
can contain any kind of objects in python

L = [1,2,3,1]
L1 = [3,2,1]

L == L1
     
False
Creating a List

# Empty
print([])
# 1D -> Homo
print([1,2,3,4,5])
# 2D
print([1,2,3,[4,5]])
# 3D
print([[[1,2],[3,4]],[[5,6],[7,8]]])
# Hetrogenous
print([1,True,5.6,5+6j,'Hello'])
# Using Type conversion
print(list('hello'))
     
[]
[1, 2, 3, 4, 5]
[1, 2, 3, [4, 5]]
[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
[1, True, 5.6, (5+6j), 'Hello']
['h', 'e', 'l', 'l', 'o']
Accessing Items from a List

# Indexing
L = [[[1,2],[3,4]],[[5,6],[7,8]]]
#positive
#print(L[0][0][1])

# Slicing
L = [1,2,3,4,5,6]

print(L[::-1])
     
[6, 5, 4, 3, 2, 1]
Adding Items to a List

# append
L = [1,2,3,4,5]
L.append(True)
print(L)
     
[1, 2, 3, 4, 5, True]

# extend
L = [1,2,3,4,5]
L.extend([6,7,8])
print(L)
     
[1, 2, 3, 4, 5, 6, 7, 8]

L = [1,2,3,4,5]
L.append([6,7,8])
print(L)
     
[1, 2, 3, 4, 5, [6, 7, 8]]

L = [1,2,3,4,5]
L.extend('delhi')
print(L)
     
[1, 2, 3, 4, 5, 'd', 'e', 'l', 'h', 'i']

# insert
L = [1,2,3,4,5]

L.insert(1,100)
print(L)
     
[1, 100, 2, 3, 4, 5]
Editing items in a List

L = [1,2,3,4,5]

# editing with indexing
L[-1] = 500

# editing with slicing
L[1:4] = [200,300,400]

print(L)
     
[1, 200, 300, 400, 500]
Deleting items from a List

# del
L = [1,2,3,4,5]

# indexing
del L[-1]

# slicing
del L[1:3]
print(L)
     
[1, 4]

# remove

L = [1,2,3,4,5]

L.remove(5)

print(L)

     
[1, 2, 3, 4]

# pop
L = [1,2,3,4,5]

L.pop()

print(L)
     
[1, 2, 3, 4]

# clear
L = [1,2,3,4,5]

L.clear()

print(L)
     
[]
Operations on Lists
Arithmetic
Membership
Loop

# Arithmetic (+ ,*)

L1 = [1,2,3,4]
L2 = [5,6,7,8]

# Concatenation/Merge
print(L1 + L2)
     
[1, 2, 3, 4, 5, 6, 7, 8]

print(L1*3)
     
[1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]

L1 = [1,2,3,4,5]
L2 = [1,2,3,4,[5,6]]

print(5 not in L1)
print([5,6] in L2)
     
False
True

# Loops
L1 = [1,2,3,4,5]
L2 = [1,2,3,4,[5,6]]
L3 = [[[1,2],[3,4]],[[5,6],[7,8]]]

for i in L3:
  print(i)
     
[[1, 2], [3, 4]]
[[5, 6], [7, 8]]
List Functions

# len/min/max/sorted
L = [2,1,5,7,0]

print(len(L))
print(min(L))
print(max(L))
print(sorted(L,reverse=True))
     
5
0
7
[7, 5, 2, 1, 0]

# count
L = [1,2,1,3,4,1,5]
L.count(5)
     
1

# index
L = [1,2,1,3,4,1,5]
L.index(1)
     
0

# reverse
L = [2,1,5,7,0]
# permanently reverses the list
L.reverse()
print(L)
     
[0, 7, 5, 1, 2]

# sort (vs sorted)
L = [2,1,5,7,0]
print(L)
print(sorted(L))
print(L)
L.sort()
print(L)
     
[2, 1, 5, 7, 0]
[0, 1, 2, 5, 7]
[2, 1, 5, 7, 0]
[0, 1, 2, 5, 7]

# copy -> shallow
L = [2,1,5,7,0]
print(L)
print(id(L))
L1 = L.copy()
print(L1)
print(id(L1))
     
[2, 1, 5, 7, 0]
140163201056112
[2, 1, 5, 7, 0]
140163201128800
List Comprehension
List Comprehension provides a concise way of creating lists.

newlist = [expression for item in iterable if condition == True]image.png

Advantages of List Comprehension

More time-efficient and space-efficient than loops.
Require fewer lines of code.
Transforms iterative statement into a formula.

# Add 1 to 10 numbers to a list
L = []

for i in range(1,11):
  L.append(i)

print(L)
     
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

L = [i for i in range(1,11)]
print(L)
     
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# scalar multiplication on a vector
v = [2,3,4]
s = -3
# [-6,-9,-12]

[s*i for i in v]
     
[-6, -9, -12]

# Add squares
L = [1,2,3,4,5]

[i**2 for i in L]

     
[1, 4, 9, 16, 25]

# Print all numbers divisible by 5 in the range of 1 to 50

[i for i in range(1,51) if i%5 == 0]
     
[5, 10, 15, 20, 25, 30, 35, 40, 45, 50]

# find languages which start with letter p
languages = ['java','python','php','c','javascript']

[language for language in languages if language.startswith('p')]
     
['python', 'php']

# Nested if with List Comprehension
basket = ['apple','guava','cherry','banana']
my_fruits = ['apple','kiwi','grapes','banana']

# add new list from my_fruits and items if the fruit exists in basket and also starts with 'a'

[fruit for fruit in my_fruits if fruit in basket if fruit.startswith('a')]
     
['apple']

# Print a (3,3) matrix using list comprehension -> Nested List comprehension
[[i*j for i in range(1,4)] for j in range(1,4)]
     
[[1, 2, 3], [2, 4, 6], [3, 6, 9]]

# cartesian products -> List comprehension on 2 lists together
L1 = [1,2,3,4]
L2 = [5,6,7,8]

[i*j for i in L1 for j in L2]
     
[5, 6, 7, 8, 10, 12, 14, 16, 15, 18, 21, 24, 20, 24, 28, 32]
2 ways to traverse a list
itemwise
indexwise

# itemwise
L = [1,2,3,4]

for i in L:
  print(i)
     
1
2
3
4

# indexwise
L = [1,2,3,4]

for i in range(0,len(L)):
  print(L[i])

     
1
2
3
4
Zip
The zip() function returns a zip object, which is an iterator of tuples where the first item in each passed iterator is paired together, and then the second item in each passed iterator are paired together.

If the passed iterators have different lengths, the iterator with the least items decides the length of the new iterator.


# Write a program to add items of 2 lists indexwise

L1 = [1,2,3,4]
L2 = [-1,-2,-3,-4]

list(zip(L1,L2))

[i+j for i,j in zip(L1,L2)]
     
[0, 0, 0, 0]

L = [1,2,print,type,input]

print(L)
     
[1, 2, <built-in function print>, <class 'type'>, <bound method Kernel.raw_input of <google.colab._kernel.Kernel object at 0x7f7a67452a90>>]
Disadvantages of Python Lists
Slow
Risky usage
eats up more memory

a = [1,2,3]
b = a.copy()

print(a)
print(b)

a.append(4)
print(a)
print(b)

# lists are mutable
     
[1, 2, 3]
[1, 2, 3]
[1, 2, 3, 4]
[1, 2, 3]
List Programs

# Create 2 lists from a given list where
# 1st list will contain all the odd numbers from the original list and
# the 2nd one will contain all the even numbers

L = [1,2,3,4,5,6]
     

# How to take list as input from user
     

# Write a program to merge 2 list without using the + operator
L1 = [1,2,3,4]
L2 = [5,6,7,8]

     

# Write a program to replace an item with a different item if found in the list
L = [1,2,3,4,5,3]
# replace 3 with 300
     

# Write a program that can convert a 2D list to 1D list
     

# Write a program to remove duplicate items from a list

L = [1,2,1,2,3,4,5,3,4]
     

# Write a program to check if a list is in ascending order or not
     



################################################################################



Tuples
A tuple in Python is similar to a list. The difference between the two is that we cannot change the elements of a tuple once it is assigned whereas we can change the elements of a list.

In short, a tuple is an immutable list. A tuple can not be changed in any way once it is created.

Characterstics

Ordered
Unchangeble
Allows duplicate
Plan of attack
Creating a Tuple
Accessing items
Editing items
Adding items
Deleting items
Operations on Tuples
Tuple Functions
Creating Tuples

# empty
t1 = ()
print(t1)
# create a tuple with a single item
t2 = ('hello',)
print(t2)
print(type(t2))
# homo
t3 = (1,2,3,4)
print(t3)
# hetro
t4 = (1,2.5,True,[1,2,3])
print(t4)
# tuple
t5 = (1,2,3,(4,5))
print(t5)
# using type conversion
t6 = tuple('hello')
print(t6)
     
()
('hello',)
<class 'tuple'>
(1, 2, 3, 4)
(1, 2.5, True, [1, 2, 3])
(1, 2, 3, (4, 5))
('h', 'e', 'l', 'l', 'o')
Accessing Items
Indexing
Slicing

print(t3)
print(t3[0])
print(t3[-1])
     
(1, 2, 3, 4)
1
4

t5[-1][0]
     
4
Editing items

print(t3)
t3[0] = 100
# immutable just like strings
     
(1, 2, 3, 4)
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-30-49d9e1416ccf> in <module>
      1 print(t3)
----> 2 t3[0] = 100

TypeError: 'tuple' object does not support item assignment
Adding items

print(t3)
# not possible
     
(1, 2, 3, 4)
Deleting items

print(t3)
del t3
print(t3)
     
(1, 2, 3, 4)
---------------------------------------------------------------------------
NameError                                 Traceback (most recent call last)
<ipython-input-33-0a67b29ad777> in <module>
      1 print(t3)
      2 del t3
----> 3 print(t3)

NameError: name 't3' is not defined

t = (1,2,3,4,5)

t[-1:-4:-1]
     
(5, 4, 3)

print(t5)
del t5[-1]
     
(1, 2, 3, (4, 5))
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-35-2b39d140e8ae> in <module>
      1 print(t5)
----> 2 del t5[-1]

TypeError: 'tuple' object doesn't support item deletion
Operations on Tuples

# + and *
t1 = (1,2,3,4)
t2 = (5,6,7,8)

print(t1 + t2)

print(t1*3)
# membership
1 in t1
# iteration
for i in t1:
  print(i)
     
(1, 2, 3, 4, 5, 6, 7, 8)
(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
1
2
3
4
Tuple Functions

# len/sum/min/max/sorted
t = (1,2,3,4)
len(t)

sum(t)

min(t)

max(t)

sorted(t,reverse=True)
     
[4, 3, 2, 1]

# count

t = (1,2,3,4,5)

t.count(50)
     
0

# index
t.index(50)
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-51-cae2b6ba49a8> in <module>
      1 # index
----> 2 t.index(50)

ValueError: tuple.index(x): x not in tuple
Difference between Lists and Tuples
Syntax
Mutability
Speed
Memory
Built in functionality
Error prone
Usability

import time

L = list(range(100000000))
T = tuple(range(100000000))

start = time.time()
for i in L:
  i*5
print('List time',time.time()-start)

start = time.time()
for i in T:
  i*5
print('Tuple time',time.time()-start)
     
List time 9.853569507598877
Tuple time 8.347511053085327

import sys

L = list(range(1000))
T = tuple(range(1000))

print('List size',sys.getsizeof(L))
print('Tuple size',sys.getsizeof(T))

     
List size 9120
Tuple size 8056

a = [1,2,3]
b = a

a.append(4)
print(a)
print(b)
     
[1, 2, 3, 4]
[1, 2, 3, 4]

a = (1,2,3)
b = a

a = a + (4,)
print(a)
print(b)
     
(1, 2, 3, 4)
(1, 2, 3)
Why use tuple?
Special Syntax

# tuple unpacking
a,b,c = (1,2,3)
print(a,b,c)
     
1 2 3

a,b = (1,2,3)
print(a,b)
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-55-22f327f11d4b> in <module>
----> 1 a,b = (1,2,3)
      2 print(a,b)

ValueError: too many values to unpack (expected 2)

a = 1
b = 2
a,b = b,a

print(a,b)
     
2 1

a,b,*others = (1,2,3,4)
print(a,b)
print(others)
     
1 2
[3, 4]

# zipping tuples
a = (1,2,3,4)
b = (5,6,7,8)

tuple(zip(a,b))
     
((1, 5), (2, 6), (3, 7), (4, 8))
Sets
A set is an unordered collection of items. Every set element is unique (no duplicates) and must be immutable (cannot be changed).

However, a set itself is mutable. We can add or remove items from it.

Sets can also be used to perform mathematical set operations like union, intersection, symmetric difference, etc.

Characterstics:

Unordered
Mutable
No Duplicates
Can't contain mutable data types
Creating Sets

# empty
s = set()
print(s)
print(type(s))
# 1D and 2D
s1 = {1,2,3}
print(s1)
#s2 = {1,2,3,{4,5}}
#print(s2)
# homo and hetro
s3 = {1,'hello',4.5,(1,2,3)}
print(s3)
# using type conversion

s4 = set([1,2,3])
print(s4)
# duplicates not allowed
s5 = {1,1,2,2,3,3}
print(s5)
# set can't have mutable items
s6 = {1,2,[3,4]}
print(s6)
     
set()
<class 'set'>
{1, 2, 3}
{1, 4.5, (1, 2, 3), 'hello'}
{1, 2, 3}
{1, 2, 3}
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-71-ab3c7dde6aed> in <module>
     19 print(s5)
     20 # set can't have mutable items
---> 21 s6 = {1,2,[3,4]}
     22 print(s6)

TypeError: unhashable type: 'list'

s1 = {1,2,3}
s2 = {3,2,1}

print(s1 == s2)
     
True


     
Accessing Items

s1 = {1,2,3,4}
s1[0:3]
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-75-4c49b6b6050d> in <module>
      1 s1 = {1,2,3,4}
----> 2 s1[0:3]

TypeError: 'set' object is not subscriptable
Editing Items

s1 = {1,2,3,4}
s1[0] = 100
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-76-bd617ce25076> in <module>
      1 s1 = {1,2,3,4}
----> 2 s1[0] = 100

TypeError: 'set' object does not support item assignment
Adding Items

S = {1,2,3,4}
# add
# S.add(5)
# print(S)
# update
S.update([5,6,7])
print(S)
     
{1, 2, 3, 4, 5, 6, 7}
Deleting Items

# del
s = {1,2,3,4,5}
# print(s)
# del s[0]
# print(s)
# discard
# s.discard(50)
# print(s)
# remove
# s.remove(50)
# print(s)
# pop
# s.pop()
# clear
s.clear()
print(s)
     
set()
Set Operation

s1 = {1,2,3,4,5}
s2 = {4,5,6,7,8}
s1 | s2
# Union(|)
# Intersection(&)
s1 & s2
# Difference(-)
s1 - s2
s2 - s1
# Symmetric Difference(^)
s1 ^ s2
# Membership Test
1 not in s1
# Iteration
for i in s1:
  print(i)
     
1
2
3
4
5
Set Functions

# len/sum/min/max/sorted
s = {3,1,4,5,2,7}
len(s)
sum(s)
min(s)
max(s)
sorted(s,reverse=True)
     
[7, 5, 4, 3, 2, 1]

# union/update
s1 = {1,2,3,4,5}
s2 = {4,5,6,7,8}

# s1 | s2
s1.union(s1)

s1.update(s2)
print(s1)
print(s2)
     
{1, 2, 3, 4, 5, 6, 7, 8}
{4, 5, 6, 7, 8}

# intersection/intersection_update
s1 = {1,2,3,4,5}
s2 = {4,5,6,7,8}

s1.intersection(s2)

s1.intersection_update(s2)
print(s1)
print(s2)
     
{4, 5}
{4, 5, 6, 7, 8}

# difference/difference_update
s1 = {1,2,3,4,5}
s2 = {4,5,6,7,8}

s1.difference(s2)

s1.difference_update(s2)
print(s1)
print(s2)
     
{1, 2, 3}
{4, 5, 6, 7, 8}

# symmetric_difference/symmetric_difference_update
s1 = {1,2,3,4,5}
s2 = {4,5,6,7,8}

s1.symmetric_difference(s2)

s1.symmetric_difference_update(s2)
print(s1)
print(s2)
     
{1, 2, 3, 6, 7, 8}
{4, 5, 6, 7, 8}

# isdisjoint/issubset/issuperset
s1 = {1,2,3,4}
s2 = {7,8,5,6}

s1.isdisjoint(s2)
     
True

s1 = {1,2,3,4,5}
s2 = {3,4,5}

s1.issuperset(s2)
     
True

# copy
s1 = {1,2,3}
s2 = s1.copy()

print(s1)
print(s2)
     
{1, 2, 3}
{1, 2, 3}
Frozenset
Frozen set is just an immutable version of a Python set object


# create frozenset
fs1 = frozenset([1,2,3])
fs2 = frozenset([3,4,5])

fs1 | fs2
     
frozenset({1, 2, 3, 4, 5})

# what works and what does not
# works -> all read functions
# does't work -> write operations
     

# When to use
# 2D sets
fs = frozenset([1,2,frozenset([3,4])])
fs
     
frozenset({1, 2, frozenset({3, 4})})
Set Comprehension

# examples

{i**2 for i in range(1,11) if i>5}
     
{36, 49, 64, 81, 100}


     
Dictionary
Dictionary in Python is a collection of keys values, used to store data values like a map, which, unlike other data types which hold only a single value as an element.

In some languages it is known as map or assosiative arrays.

dict = { 'name' : 'nitish' , 'age' : 33 , 'gender' : 'male' }

Characterstics:

Mutable
Indexing has no meaning
keys can't be duplicated
keys can't be mutable items
Create Dictionary

# empty dictionary
d = {}
d
# 1D dictionary
d1 = { 'name' : 'nitish' ,'gender' : 'male' }
d1
# with mixed keys
d2 = {(1,2,3):1,'hello':'world'}
d2
# 2D dictionary -> JSON
s = {
    'name':'nitish',
     'college':'bit',
     'sem':4,
     'subjects':{
         'dsa':50,
         'maths':67,
         'english':34
     }
}
s
# using sequence and dict function
d4 = dict([('name','nitish'),('age',32),(3,3)])
d4
# duplicate keys
d5 = {'name':'nitish','name':'rahul'}
d5
# mutable items as keys
d6 = {'name':'nitish',(1,2,3):2}
print(d6)
     
{'name': 'nitish', (1, 2, 3): 2}
Accessing items

my_dict = {'name': 'Jack', 'age': 26}
# []
my_dict['age']
# get
my_dict.get('age')

s['subjects']['maths']
     
67
Adding key-value pair

d4['gender'] = 'male'
d4
d4['weight'] = 72
d4

s['subjects']['ds'] = 75
s
     
{'name': 'nitish',
 'college': 'bit',
 'sem': 4,
 'subjects': {'dsa': 50, 'maths': 67, 'english': 34, 'ds': 75}}
Remove key-value pair

d = {'name': 'nitish', 'age': 32, 3: 3, 'gender': 'male', 'weight': 72}
# pop
#d.pop(3)
#print(d)
# popitem
#d.popitem()
# d.popitem()
# print(d)
# del
#del d['name']
#print(d)
# clear
d.clear()
print(d)

del s['subjects']['maths']
s
     
{}
{'name': 'nitish',
 'college': 'bit',
 'sem': 4,
 'subjects': {'dsa': 50, 'english': 34, 'ds': 75}}
Editing key-value pair

s['subjects']['dsa'] = 80
s
     
{'name': 'nitish',
 'college': 'bit',
 'sem': 5,
 'subjects': {'dsa': 80, 'english': 34, 'ds': 75}}
Dictionary Operations
Membership
Iteration

print(s)

'name' in s
     
{'name': 'nitish', 'college': 'bit', 'sem': 5, 'subjects': {'dsa': 80, 'english': 34, 'ds': 75}}
True

d = {'name':'nitish','gender':'male','age':33}

for i in d:
  print(i,d[i])
     
name nitish
gender male
age 33
Dictionary Functions

# len/sorted
len(d)
print(d)
sorted(d,reverse=True)
max(d)
     
{'name': 'nitish', 'gender': 'male', 'age': 33}
'name'

# items/keys/values
print(d)

print(d.items())
print(d.keys())
print(d.values())
     
{'name': 'nitish', 'gender': 'male', 'age': 33}
dict_items([('name', 'nitish'), ('gender', 'male'), ('age', 33)])
dict_keys(['name', 'gender', 'age'])
dict_values(['nitish', 'male', 33])

# update
d1 = {1:2,3:4,4:5}
d2 = {4:7,6:8}

d1.update(d2)
print(d1)
     
{1: 2, 3: 4, 4: 7, 6: 8}
Dictionary Comprehension
image.png


# print 1st 10 numbers and their squares
{i:i**2 for i in range(1,11)}
     
{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100}

distances = {'delhi':1000,'mumbai':2000,'bangalore':3000}
print(distances.items())
     
dict_items([('delhi', 1000), ('mumbai', 2000), ('bangalore', 3000)])

# using existing dict
distances = {'delhi':1000,'mumbai':2000,'bangalore':3000}
{key:value*0.62 for (key,value) in distances.items()}
     
{'delhi': 620.0, 'mumbai': 1240.0, 'bangalore': 1860.0}

# using zip
days = ["Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
temp_C = [30.5,32.6,31.8,33.4,29.8,30.2,29.9]

{i:j for (i,j) in zip(days,temp_C)}
     
{'Sunday': 30.5,
 'Monday': 32.6,
 'Tuesday': 31.8,
 'Wednesday': 33.4,
 'Thursday': 29.8,
 'Friday': 30.2,
 'Saturday': 29.9}

# using if condition
products = {'phone':10,'laptop':0,'charger':32,'tablet':0}

{key:value for (key,value) in products.items() if value>0}
     
{'phone': 10, 'charger': 32}

# Nested Comprehension
# print tables of number from 2 to 4
{i:{j:i*j for j in range(1,11)} for i in range(2,5)}
     
{2: {1: 2, 2: 4, 3: 6, 4: 8, 5: 10, 6: 12, 7: 14, 8: 16, 9: 18, 10: 20},
 3: {1: 3, 2: 6, 3: 9, 4: 12, 5: 15, 6: 18, 7: 21, 8: 24, 9: 27, 10: 30},
 4: {1: 4, 2: 8, 3: 12, 4: 16, 5: 20, 6: 24, 7: 28, 8: 32, 9: 36, 10: 40}}

{
    2:{1:2,2:4,3:6,4:8},
    3:{1:3,2:6,3:9,4:12},
    4:{1:4,2:8,3:12,4:16}
}
     


##################################################################################




Let's create a function(with docstring)

def is_even(num):
  """
  This function returns if a given number is odd or even
  input - any valid integer
  output - odd/even
  created on - 16th Nov 2022
  """
  if type(num) == int:
    if num % 2 == 0:
      return 'even'
    else:
      return 'odd'
  else:
    return 'pagal hai kya?'
     

# function
# function_name(input)
for i in range(1,11):
  x = is_even(i)
  print(x)
     
odd
even
odd
even
odd
even
odd
even
odd
even

print(type.__doc__)
     
type(object_or_name, bases, dict)
type(object) -> the object's type
type(name, bases, dict) -> a new type
2 Point of views

is_even('hello')
     
'pagal hai kya?'
Parameters Vs Arguments
Types of Arguments
Default Argument
Positional Argument
Keyword Argument

def power(a=1,b=1):
  return a**b
     

power()
     
1

# positional argument
power(2,3)
     
8

# keyword argument
power(b=3,a=2)
     
8
*args and **kwargs
*args and **kwargs are special Python keywords that are used to pass the variable length of arguments to a function


# *args
# allows us to pass a variable number of non-keyword arguments to a function.

def multiply(*kwargs):
  product = 1

  for i in kwargs:
    product = product * i

  print(kwargs)
  return product
     

multiply(1,2,3,4,5,6,7,8,9,10,12)
     
(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12)
43545600

# **kwargs
# **kwargs allows us to pass any number of keyword arguments.
# Keyword arguments mean that they contain a key-value pair, like a Python dictionary.

def display(**salman):

  for (key,value) in salman.items():
    print(key,'->',value)

     

display(india='delhi',srilanka='colombo',nepal='kathmandu',pakistan='islamabad')
     
india -> delhi
srilanka -> colombo
nepal -> kathmandu
pakistan -> islamabad
Points to remember while using *args and **kwargs
order of the arguments matter(normal -> *args -> **kwargs)
The words “args” and “kwargs” are only a convention, you can use any name of your choice


     
How Functions are executed in memory?


     
Without return statement

L = [1,2,3]
print(L.append(4))
print(L)
     
None
[1, 2, 3, 4]
Variable Scope

def g(y):
    print(x)
    print(x+1)
x = 5
g(x)
print(x)
     

def f(y):
    x = 1
    x += 1
    print(x)
x = 5
f(x)
print(x)
     

def h(y):
    x += 1
x = 5
h(x)
print(x)
     

def f(x):
   x = x + 1
   print('in f(x): x =', x)
   return x

x = 3
z = f(x)
print('in main program scope: z =', z)
print('in main program scope: x =', x)
     
Nested Functions

def f():
  def g():
    print('inside function g')
    f()
  g()
  print('inside function f')
     

f()
     


inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
inside function g
---------------------------------------------------------------------------
RecursionError                            Traceback (most recent call last)
<ipython-input-92-c43e34e6d405> in <module>
----> 1 f()

<ipython-input-91-374a68ddd49e> in f()
      3     print('inside function g')
      4     f()
----> 5   g()
      6   print('inside function f')

<ipython-input-91-374a68ddd49e> in g()
      2   def g():
      3     print('inside function g')
----> 4     f()
      5   g()
      6   print('inside function f')

... last 2 frames repeated, from the frame below ...

<ipython-input-91-374a68ddd49e> in f()
      3     print('inside function g')
      4     f()
----> 5   g()
      6   print('inside function f')

RecursionError: maximum recursion depth exceeded

def g(x):
    def h():
        x = 'abc'
    x = x + 1
    print('in g(x): x =', x)
    h()
    return x

x = 3
z = g(x)
     

def g(x):
    def h(x):
        x = x+1
        print("in h(x): x = ", x)
    x = x + 1
    print('in g(x): x = ', x)
    h(x)
    return x

x = 3
z = g(x)
print('in main program scope: x = ', x)
print('in main program scope: z = ', z)
     


     
Functions are 1st class citizens

# type and id
def square(num):
  return num**2

type(square)

id(square)
     
140471717004784

# reassign
x = square
id(x)
x(3)
     
9

a = 2
b = a
b
     
2

# deleting a function
del square
     

square(3)
     
---------------------------------------------------------------------------
NameError                                 Traceback (most recent call last)
<ipython-input-104-2cfd8bba3a88> in <module>
----> 1 square(3)

NameError: name 'square' is not defined

# storing
L = [1,2,3,4,square]
L[-1](3)
     
9

s = {square}
s
     
{<function __main__.square(num)>}

# returning a function

     


     

def f():
    def x(a, b):
        return a+b
    return x

val = f()(3,4)
print(val)
     

# function as argument
     

def func_a():
    print('inside func_a')

def func_b(z):
    print('inside func_c')
    return z()

print(func_b(func_a))
     
Benefits of using a Function
Code Modularity
Code Readibility
Code Reusability
Lambda Function
A lambda function is a small anonymous function.

A lambda function can take any number of arguments, but can only have one expression.

image.png


# x -> x^2
lambda x:x**2
     
<function __main__.<lambda>(x)>

# x,y -> x+y
a = lambda x,y:x+y
a(5,2)
     
7
Diff between lambda vs Normal Function
No name
lambda has no return value(infact,returns a function)
lambda is written in 1 line
not reusable
Then why use lambda functions?
They are used with HOF


# check if a string has 'a'
a = lambda s:'a' in s
a('hello')
     
False

# odd or even
a = lambda x:'even' if x%2 == 0 else 'odd'
a(6)
     
'even'
Higher Order Functions

# Example

def square(x):
  return x**2

def cube(x):
  return x**3

# HOF
def transform(f,L):
  output = []
  for i in L:
    output.append(f(i))

  print(output)

L = [1,2,3,4,5]

transform(lambda x:x**3,L)
     
[1, 8, 27, 64, 125]
Map

# square the items of a list
list(map(lambda x:x**2,[1,2,3,4,5]))
     
[1, 4, 9, 16, 25]

# odd/even labelling of list items
L = [1,2,3,4,5]
list(map(lambda x:'even' if x%2 == 0 else 'odd',L))
     
['odd', 'even', 'odd', 'even', 'odd']

# fetch names from a list of dict

users = [
    {
        'name':'Rahul',
        'age':45,
        'gender':'male'
    },
    {
        'name':'Nitish',
        'age':33,
        'gender':'male'
    },
    {
        'name':'Ankita',
        'age':50,
        'gender':'female'
    }
]

list(map(lambda users:users['gender'],users))
     
['male', 'male', 'female']
Filter

# numbers greater than 5
L = [3,4,5,6,7]

list(filter(lambda x:x>5,L))
     
[6, 7]

# fetch fruits starting with 'a'
fruits = ['apple','guava','cherry']

list(filter(lambda x:x.startswith('a'),fruits))
     
['apple']
Reduce

# sum of all item
import functools

functools.reduce(lambda x,y:x+y,[1,2,3,4,5])
     
15

# find min
functools.reduce(lambda x,y:x if x>y else y,[23,11,45,10,1])
     
45


     
##################################################################################





L = [1,2,3]

L.upper()
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-1-af1f83522ab7> in <module>
      1 L = [1,2,3]
      2 
----> 3 L.upper()

AttributeError: 'list' object has no attribute 'upper'

s = 'hello'
s.append('x')
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-2-2cb7c5babec0> in <module>
      1 s = 'hello'
----> 2 s.append('x')

AttributeError: 'str' object has no attribute 'append'

L = [1,2,3]
print(type(L))
     
<class 'list'>

s = [1,2,3]
     

# syntax to create an object

#objectname = classname()
     

# object literal
L = [1,2,3]
     

L = list()
L
     
[]

s = str()
s
     
''

# Pascal Case

HelloWorld
     

class Atm:

  # constructor(special function)->superpower ->
  def __init__(self):
    print(id(self))
    self.pin = ''
    self.balance = 0
    #self.menu()

  def menu(self):
    user_input = input("""
    Hi how can I help you?
    1. Press 1 to create pin
    2. Press 2 to change pin
    3. Press 3 to check balance
    4. Press 4 to withdraw
    5. Anything else to exit
    """)

    if user_input == '1':
      self.create_pin()
    elif user_input == '2':
      self.change_pin()
    elif user_input == '3':
      self.check_balance()
    elif user_input == '4':
      self.withdraw()
    else:
      exit()

  def create_pin(self):
    user_pin = input('enter your pin')
    self.pin = user_pin

    user_balance = int(input('enter balance'))
    self.balance = user_balance

    print('pin created successfully')
    self.menu()

  def change_pin():
    old_pin = input('enter old pin')

    if old_pin == self.pin:
      # let him change the pin
      new_pin = input('enter new pin')
      self.pin = new_pin
      print('pin change successful')
      self.menu()
    else:
      print('nai karne de sakta re baba')
      self.menu()

  def check_balance(self):
    user_pin = input('enter your pin')
    if user_pin == self.pin:
      print('your balance is ',self.balance)
    else:
      print('chal nikal yahan se')

  def withdraw(self):
    user_pin = input('enter the pin')
    if user_pin == self.pin:
      # allow to withdraw
      amount = int(input('enter the amount'))
      if amount <= self.balance:
        self.balance = self.balance - amount
        print('withdrawl successful.balance is',self.balance)
      else:
        print('abe garib')
    else:
      print('sale chor')
    self.menu()



     

obj1 = Atm()
     
140289660099024

id(obj1)
     
140289660099024

obj2 = Atm()
     
140289660586384

id(obj2)
     
140289660586384

L = [1,2,3]
len(L) # function ->bcos it is outside the list class
L.append()# method -> bcos it is inside the list class
     

class Temp:

  def __init__(self):
    print('hello')

obj = Temp()
     
hello

3/4*1/2
     
0.375

class Fraction:

  # parameterized constructor
  def __init__(self,x,y):
    self.num = x
    self.den = y

  def __str__(self):
    return '{}/{}'.format(self.num,self.den)

  def __add__(self,other):
    new_num = self.num*other.den + other.num*self.den
    new_den = self.den*other.den

    return '{}/{}'.format(new_num,new_den)

  def __sub__(self,other):
    new_num = self.num*other.den - other.num*self.den
    new_den = self.den*other.den

    return '{}/{}'.format(new_num,new_den)

  def __mul__(self,other):
    new_num = self.num*other.num
    new_den = self.den*other.den

    return '{}/{}'.format(new_num,new_den)

  def __truediv__(self,other):
    new_num = self.num*other.den
    new_den = self.den*other.num

    return '{}/{}'.format(new_num,new_den)

  def convert_to_decimal(self):
    return self.num/self.den





     

fr1 = Fraction(3,4)
fr2 = Fraction(1,2)
     

fr1.convert_to_decimal()
# 3/4
     
0.75

print(fr1 + fr2)
print(fr1 - fr2)
print(fr1 * fr2)
print(fr1 / fr2)
     
10/8
2/8
3/8
6/4

s1={1,2,3}
s2={3,4,5}

s1 + s2
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-32-3a417afc75fb> in <module>
      2 s2={3,4,5}
      3 
----> 4 s1 + s2

TypeError: unsupported operand type(s) for +: 'set' and 'set'

print(fr1 - fr2)
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-39-929bcd8b32dc> in <module>
----> 1 print(fr1 - fr2)

TypeError: unsupported operand type(s) for -: 'Fraction' and 'Fraction'


     
###################################################################################



Write OOP classes to handle the following scenarios:
A user can create and view 2D coordinates
A user can find out the distance between 2 coordinates
A user can find find the distance of a coordinate from origin
A user can check if a point lies on a given line
A user can find the distance between a given 2D point and a given line

class Point:

  def __init__(self,x,y):
    self.x_cod = x
    self.y_cod = y

  def __str__(self):
    return '<{},{}>'.format(self.x_cod,self.y_cod)

  def euclidean_distance(self,other):
    return ((self.x_cod - other.x_cod)**2 + (self.y_cod - other.y_cod)**2)**0.5

  def distance_from_origin(self):
    return (self.x_cod**2 + self.y_cod**2)**0.5
    # return self.euclidean_distance(Point(0,0))


class Line:

  def __init__(self,A,B,C):
    self.A = A
    self.B = B
    self.C = C

  def __str__(self):
    return '{}x + {}y + {} = 0'.format(self.A,self.B,self.C)

  def point_on_line(line,point):
    if line.A*point.x_cod + line.B*point.y_cod + line.C == 0:
      return "lies on the line"
    else:
      return "does not lie on the line"

  def shortest_distance(line,point):
    return abs(line.A*point.x_cod + line.B*point.y_cod + line.C)/(line.A**2 + line.B**2)**0.5

     

l1 = Line(1,1,-2)
p1 = Point(1,10)
print(l1)
print(p1)

l1.shortest_distance(p1)
     
1x + 1y + -2 = 0
<1,10>
6.363961030678928
How objects access attributes

class Person:

  def __init__(self,name_input,country_input):
    self.name = name_input
    self.country = country_input

  def greet(self):
    if self.country == 'india':
      print('Namaste',self.name)
    else:
      print('Hello',self.name)

     

# how to access attributes
p = Person('nitish','india')
     

p.name
     
'nitish'

# how to access methods
p.greet()
     
Namaste nitish

# what if i try to access non-existent attributes
p.gender
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-49-39388d77d830> in <module>
      1 # what if i try to access non-existent attributes
----> 2 p.gender

AttributeError: 'Person' object has no attribute 'gender'
Attribute creation from outside of the class

p.gender = 'male'
     

p.gender
     
'male'
Reference Variables
Reference variables hold the objects
We can create objects without reference variable as well
An object can have multiple reference variables
Assigning a new reference variable to an existing object does not create a new object

# object without a reference
class Person:

  def __init__(self):
    self.name = 'nitish'
    self.gender = 'male'

p = Person()
q = p
     

# Multiple ref
print(id(p))
print(id(q))
     
140655538334992
140655538334992

# change attribute value with the help of 2nd object
     

print(p.name)
print(q.name)
q.name = 'ankit'
print(q.name)
print(p.name)
     
nitish
nitish
ankit
ankit
Pass by reference

class Person:

  def __init__(self,name,gender):
    self.name = name
    self.gender = gender

# outside the class -> function
def greet(person):
  print('Hi my name is',person.name,'and I am a',person.gender)
  p1 = Person('ankit','male')
  return p1

p = Person('nitish','male')
x = greet(p)
print(x.name)
print(x.gender)
     
Hi my name is nitish and I am a male
ankit
male

class Person:

  def __init__(self,name,gender):
    self.name = name
    self.gender = gender

# outside the class -> function
def greet(person):
  print(id(person))
  person.name = 'ankit'
  print(person.name)

p = Person('nitish','male')
print(id(p))
greet(p)
print(p.name)
     
140655538334288
140655538334288
ankit
ankit
Object ki mutability

class Person:

  def __init__(self,name,gender):
    self.name = name
    self.gender = gender

# outside the class -> function
def greet(person):
  person.name = 'ankit'
  return person

p = Person('nitish','male')
print(id(p))
p1 = greet(p)
print(id(p1))
     
140655555218960
140655555218960
Encapsulation

# instance var -> python tutor
class Person:

  def __init__(self,name_input,country_input):
    self.name = name_input
    self.country = country_input

p1 = Person('nitish','india')
p2 = Person('steve','australia')
     

p2.name
     
'steve'

class Atm:

  # constructor(special function)->superpower ->
  def __init__(self):
    print(id(self))
    self.pin = ''
    self.__balance = 0
    #self.menu()

  def get_balance(self):
    return self.__balance

  def set_balance(self,new_value):
    if type(new_value) == int:
      self.__balance = new_value
    else:
      print('beta bahot maarenge')

  def __menu(self):
    user_input = input("""
    Hi how can I help you?
    1. Press 1 to create pin
    2. Press 2 to change pin
    3. Press 3 to check balance
    4. Press 4 to withdraw
    5. Anything else to exit
    """)

    if user_input == '1':
      self.create_pin()
    elif user_input == '2':
      self.change_pin()
    elif user_input == '3':
      self.check_balance()
    elif user_input == '4':
      self.withdraw()
    else:
      exit()

  def create_pin(self):
    user_pin = input('enter your pin')
    self.pin = user_pin

    user_balance = int(input('enter balance'))
    self.__balance = user_balance

    print('pin created successfully')

  def change_pin(self):
    old_pin = input('enter old pin')

    if old_pin == self.pin:
      # let him change the pin
      new_pin = input('enter new pin')
      self.pin = new_pin
      print('pin change successful')
    else:
      print('nai karne de sakta re baba')

  def check_balance(self):
    user_pin = input('enter your pin')
    if user_pin == self.pin:
      print('your balance is ',self.__balance)
    else:
      print('chal nikal yahan se')

  def withdraw(self):
    user_pin = input('enter the pin')
    if user_pin == self.pin:
      # allow to withdraw
      amount = int(input('enter the amount'))
      if amount <= self.__balance:
        self.__balance = self.__balance - amount
        print('withdrawl successful.balance is',self.__balance)
      else:
        print('abe garib')
    else:
      print('sale chor')
     

obj = Atm()
     
140655538526416

obj.get_balance()
     
1000

obj.set_balance(1000)
     

obj.withdraw()
     
enter the pin
enter the amount5000
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-93-826ea677aa70> in <module>
----> 1 obj.withdraw()

<ipython-input-86-f5bffac7e2a0> in withdraw(self)
     67       # allow to withdraw
     68       amount = int(input('enter the amount'))
---> 69       if amount <= self.__balance:
     70         self.__balance = self.__balance - amount
     71         print('withdrawl successful.balance is',self.__balance)

TypeError: '<=' not supported between instances of 'int' and 'str'
Collection of objects


     

# list of objects
class Person:

  def __init__(self,name,gender):
    self.name = name
    self.gender = gender

p1 = Person('nitish','male')
p2 = Person('ankit','male')
p3 = Person('ankita','female')

L = [p1,p2,p3]

for i in L:
  print(i.name,i.gender)
     
nitish male
ankit male
ankita female

# dict of objects
# list of objects
class Person:

  def __init__(self,name,gender):
    self.name = name
    self.gender = gender

p1 = Person('nitish','male')
p2 = Person('ankit','male')
p3 = Person('ankita','female')

d = {'p1':p1,'p2':p2,'p3':p3}

for i in d:
  print(d[i].gender)
     
male
male
female
Static Variables(Vs Instance variables)

# need for static vars
     

class Atm:

  __counter = 1

  # constructor(special function)->superpower ->
  def __init__(self):
    print(id(self))
    self.pin = ''
    self.__balance = 0
    self.cid = Atm.__counter
    Atm.__counter = Atm.__counter + 1
    #self.menu()

  # utility functions
  @staticmethod
  def get_counter():
    return Atm.__counter


  def get_balance(self):
    return self.__balance

  def set_balance(self,new_value):
    if type(new_value) == int:
      self.__balance = new_value
    else:
      print('beta bahot maarenge')

  def __menu(self):
    user_input = input("""
    Hi how can I help you?
    1. Press 1 to create pin
    2. Press 2 to change pin
    3. Press 3 to check balance
    4. Press 4 to withdraw
    5. Anything else to exit
    """)

    if user_input == '1':
      self.create_pin()
    elif user_input == '2':
      self.change_pin()
    elif user_input == '3':
      self.check_balance()
    elif user_input == '4':
      self.withdraw()
    else:
      exit()

  def create_pin(self):
    user_pin = input('enter your pin')
    self.pin = user_pin

    user_balance = int(input('enter balance'))
    self.__balance = user_balance

    print('pin created successfully')

  def change_pin(self):
    old_pin = input('enter old pin')

    if old_pin == self.pin:
      # let him change the pin
      new_pin = input('enter new pin')
      self.pin = new_pin
      print('pin change successful')
    else:
      print('nai karne de sakta re baba')

  def check_balance(self):
    user_pin = input('enter your pin')
    if user_pin == self.pin:
      print('your balance is ',self.__balance)
    else:
      print('chal nikal yahan se')

  def withdraw(self):
    user_pin = input('enter the pin')
    if user_pin == self.pin:
      # allow to withdraw
      amount = int(input('enter the amount'))
      if amount <= self.__balance:
        self.__balance = self.__balance - amount
        print('withdrawl successful.balance is',self.__balance)
      else:
        print('abe garib')
    else:
      print('sale chor')
     

c1 = Atm()
     
140655538287248

Atm.get_counter()
     
2

c3 = Atm()
     
140655538226704

c3.cid
     
3

Atm.counter
     
4
Static methods


     
Points to remember about static
Static attributes are created at class level.
Static attributes are accessed using ClassName.
Static attributes are object independent. We can access them without creating instance (object) of the class in which they are defined.
The value stored in static attribute is shared between all instances(objects) of the class in which the static attribute is defined.

class Lion:
  __water_source="well in the circus"

  def __init__(self,name, gender):
      self.__name=name
      self.__gender=gender

  def drinks_water(self):
      print(self.__name,
      "drinks water from the",Lion.__water_source)

  @staticmethod
  def get_water_source():
      return Lion.__water_source

simba=Lion("Simba","Male")
simba.drinks_water()
print( "Water source of lions:",Lion.get_water_source())
     

##################################################################################




Class Relationships
Aggregation
Inheritance
Aggregation(Has-A relationship)

# example
class Customer:

  def __init__(self,name,gender,address):
    self.name = name
    self.gender = gender
    self.address = address

  def print_address(self):
    print(self.address._Address__city,self.address.pin,self.address.state)

  def edit_profile(self,new_name,new_city,new_pin,new_state):
    self.name = new_name
    self.address.edit_address(new_city,new_pin,new_state)

class Address:

  def __init__(self,city,pin,state):
      self.__city = city
      self.pin = pin
      self.state = state

  def get_city(self):
    return self.__city

  def edit_address(self,new_city,new_pin,new_state):
    self.__city = new_city
    self.pin = new_pin
    self.state = new_state

add1 = Address('gurgaon',122011,'haryana')
cust = Customer('nitish','male',add1)

cust.print_address()

cust.edit_profile('ankit','mumbai',111111,'maharastra')
cust.print_address()
# method example
# what about private attribute
     
gurgaon 122011 haryana
mumbai 111111 maharastra
Aggregation class diagram


     
Inheritance
What is inheritance
Example
What gets inherited?

# Inheritance and it's benefits
     

# Example

# parent
class User:

  def __init__(self):
    self.name = 'nitish'
    self.gender = 'male'

  def login(self):
    print('login')

# child
class Student(User):

  def __init__(self):
    self.rollno = 100

  def enroll(self):
    print('enroll into the course')

u = User()
s = Student()

print(s.name)
s.login()
s.enroll()
     
nitish
login
enroll into the course

# Class diagram
     
What gets inherited?
Constructor
Non Private Attributes
Non Private Methods

# constructor example

class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    pass

s=SmartPhone(20000, "Apple", 13)
s.buy()
     
Inside phone constructor
Buying a phone

# constructor example 2

class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

class SmartPhone(Phone):
    def __init__(self, os, ram):
        self.os = os
        self.ram = ram
        print ("Inside SmartPhone constructor")

s=SmartPhone("Android", 2)
s.brand
     
Inside SmartPhone constructor
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-27-fff5c9f9674f> in <module>
     15 
     16 s=SmartPhone("Android", 2)
---> 17 s.brand

AttributeError: 'SmartPhone' object has no attribute 'brand'

# child can't access private members of the class

class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    #getter
    def show(self):
        print (self.__price)

class SmartPhone(Phone):
    def check(self):
        print(self.__price)

s=SmartPhone(20000, "Apple", 13)
s.show()
     
Inside phone constructor
20000

class Parent:

    def __init__(self,num):
        self.__num=num

    def get_num(self):
        return self.__num

class Child(Parent):

    def show(self):
        print("This is in child class")

son=Child(100)
print(son.get_num())
son.show()
     
100
This is in child class

class Parent:

    def __init__(self,num):
        self.__num=num

    def get_num(self):
        return self.__num

class Child(Parent):

    def __init__(self,val,num):
        self.__val=val

    def get_val(self):
        return self.__val

son=Child(100,10)
print("Parent: Num:",son.get_num())
print("Child: Val:",son.get_val())
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-35-5a17300f6fc7> in <module>
     16 
     17 son=Child(100,10)
---> 18 print("Parent: Num:",son.get_num())
     19 print("Child: Val:",son.get_val())

<ipython-input-35-5a17300f6fc7> in get_num(self)
      5 
      6     def get_num(self):
----> 7         return self.__num
      8 
      9 class Child(Parent):

AttributeError: 'Child' object has no attribute '_Parent__num'

class A:
    def __init__(self):
        self.var1=100

    def display1(self,var1):
        print("class A :", self.var1)
class B(A):

    def display2(self,var1):
        print("class B :", self.var1)

obj=B()
obj.display1(200)
     
class A : 200

# Method Overriding
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    def buy(self):
        print ("Buying a smartphone")

s=SmartPhone(20000, "Apple", 13)

s.buy()
     
Inside phone constructor
Buying a smartphone
Super Keyword

class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    def buy(self):
        print ("Buying a smartphone")
        # syntax to call parent ka buy method
        super().buy()

s=SmartPhone(20000, "Apple", 13)

s.buy()
     
Inside phone constructor
Buying a smartphone
Buying a phone

# using super outside the class
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    def buy(self):
        print ("Buying a smartphone")
        # syntax to call parent ka buy method
        super().buy()

s=SmartPhone(20000, "Apple", 13)

s.buy()
     
Inside phone constructor
---------------------------------------------------------------------------
RuntimeError                              Traceback (most recent call last)
<ipython-input-42-b20080504d0e> in <module>
     17 s=SmartPhone(20000, "Apple", 13)
     18 
---> 19 super().buy()

RuntimeError: super(): no arguments

# can super access parent ka data?
# using super outside the class
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    def buy(self):
        print ("Buying a smartphone")
        # syntax to call parent ka buy method
        print(super().brand)

s=SmartPhone(20000, "Apple", 13)

s.buy()
     
Inside phone constructor
Buying a smartphone
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-43-87cd65570d46> in <module>
     19 s=SmartPhone(20000, "Apple", 13)
     20 
---> 21 s.buy()

<ipython-input-43-87cd65570d46> in buy(self)
     15         print ("Buying a smartphone")
     16         # syntax to call parent ka buy method
---> 17         print(super().brand)
     18 
     19 s=SmartPhone(20000, "Apple", 13)

AttributeError: 'super' object has no attribute 'brand'

# super -> constuctor
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

class SmartPhone(Phone):
    def __init__(self, price, brand, camera, os, ram):
        print('Inside smartphone constructor')
        super().__init__(price, brand, camera)
        self.os = os
        self.ram = ram
        print ("Inside smartphone constructor")

s=SmartPhone(20000, "Samsung", 12, "Android", 2)

print(s.os)
print(s.brand)
     
Inside smartphone constructor
Inside phone constructor
Inside smartphone constructor
Android
Samsung
Inheritance in summary
A class can inherit from another class.

Inheritance improves code reuse

Constructor, attributes, methods get inherited to the child class

The parent has no access to the child class

Private properties of parent are not accessible directly in child class

Child class can override the attributes or methods. This is called method overriding

super() is an inbuilt function which is used to invoke the parent class methods and constructor


class Parent:

    def __init__(self,num):
      self.__num=num

    def get_num(self):
      return self.__num

class Child(Parent):

    def __init__(self,num,val):
      super().__init__(num)
      self.__val=val

    def get_val(self):
      return self.__val

son=Child(100,200)
print(son.get_num())
print(son.get_val())
     
100
200

class Parent:
    def __init__(self):
        self.num=100

class Child(Parent):

    def __init__(self):
        super().__init__()
        self.var=200

    def show(self):
        print(self.num)
        print(self.var)

son=Child()
son.show()
     
100
200

class Parent:
    def __init__(self):
        self.__num=100

    def show(self):
        print("Parent:",self.__num)

class Child(Parent):
    def __init__(self):
        super().__init__()
        self.__var=10

    def show(self):
        print("Child:",self.__var)

obj=Child()
obj.show()
     
Child: 10

class Parent:
    def __init__(self):
        self.__num=100

    def show(self):
        print("Parent:",self.__num)

class Child(Parent):
    def __init__(self):
        super().__init__()
        self.__var=10

    def show(self):
        print("Child:",self.__var)

obj=Child()
obj.show()
     
Child: 10
Types of Inheritance
Single Inheritance
Multilevel Inheritance
Hierarchical Inheritance
Multiple Inheritance(Diamond Problem)
Hybrid Inheritance

# single inheritance
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    pass

SmartPhone(1000,"Apple","13px").buy()
     
Inside phone constructor
Buying a phone

# multilevel
class Product:
    def review(self):
        print ("Product customer review")

class Phone(Product):
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    pass

s=SmartPhone(20000, "Apple", 12)

s.buy()
s.review()
     
Inside phone constructor
Buying a phone
Product customer review

# Hierarchical
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class SmartPhone(Phone):
    pass

class FeaturePhone(Phone):
    pass

SmartPhone(1000,"Apple","13px").buy()
FeaturePhone(10,"Lava","1px").buy()
     
Inside phone constructor
Buying a phone
Inside phone constructor
Buying a phone

# Multiple
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class Product:
    def review(self):
        print ("Customer review")

class SmartPhone(Phone, Product):
    pass

s=SmartPhone(20000, "Apple", 12)

s.buy()
s.review()

     
Inside phone constructor
Buying a phone
Customer review

# the diamond problem
# https://stackoverflow.com/questions/56361048/what-is-the-diamond-problem-in-python-and-why-its-not-appear-in-python2
class Phone:
    def __init__(self, price, brand, camera):
        print ("Inside phone constructor")
        self.__price = price
        self.brand = brand
        self.camera = camera

    def buy(self):
        print ("Buying a phone")

class Product:
    def buy(self):
        print ("Product buy method")

# Method resolution order
class SmartPhone(Phone,Product):
    pass

s=SmartPhone(20000, "Apple", 12)

s.buy()
     
Inside phone constructor
Buying a phone

class A:

    def m1(self):
        return 20

class B(A):

    def m1(self):
        return 30

    def m2(self):
        return 40

class C(B):

    def m2(self):
        return 20
obj1=A()
obj2=B()
obj3=C()
print(obj1.m1() + obj3.m1()+ obj3.m2())
     
70

class A:

    def m1(self):
        return 20

class B(A):

    def m1(self):
        val=super().m1()+30
        return val

class C(B):

    def m1(self):
        val=self.m1()+20
        return val
obj=C()
print(obj.m1())
     
---------------------------------------------------------------------------
RecursionError                            Traceback (most recent call last)
<ipython-input-56-bb3659d52487> in <module>
     16         return val
     17 obj=C()
---> 18 print(obj.m1())

<ipython-input-56-bb3659d52487> in m1(self)
     13 
     14     def m1(self):
---> 15         val=self.m1()+20
     16         return val
     17 obj=C()

... last 1 frames repeated, from the frame below ...

<ipython-input-56-bb3659d52487> in m1(self)
     13 
     14     def m1(self):
---> 15         val=self.m1()+20
     16         return val
     17 obj=C()

RecursionError: maximum recursion depth exceeded
Polymorphism
Method Overriding
Method Overloading
Operator Overloading

class Shape:

  def area(self,a,b=0):
    if b == 0:
      return 3.14*a*a
    else:
      return a*b

s = Shape()

print(s.area(2))
print(s.area(3,4))
     
12.56
12

'hello' + 'world'
     
'helloworld'

4 + 5
     
9

[1,2,3] + [4,5]
     
[1, 2, 3, 4, 5]
Abstraction

from abc import ABC,abstractmethod
class BankApp(ABC):

  def database(self):
    print('connected to database')

  @abstractmethod
  def security(self):
    pass

  @abstractmethod
  def display(self):
    pass

     

class MobileApp(BankApp):

  def mobile_login(self):
    print('login into mobile')

  def security(self):
    print('mobile security')

  def display(self):
    print('display')
     

mob = MobileApp()
     

mob.security()
     
mobile security

obj = BankApp()
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-24-0aa75fd04378> in <module>
----> 1 obj = BankApp()

TypeError: Can't instantiate abstract class BankApp with abstract methods display, security


     
##################################################################################



Some Theory
Types of data used for I/O:
Text - '12345' as a sequence of unicode chars
Binary - 12345 as a sequence of bytes of its binary equivalent
Hence there are 2 file types to deal with
Text files - All program files are text files
Binary Files - Images,music,video,exe files
How File I/O is done in most programming languages
Open a file
Read/Write data
Close the file
Writing to a file

# case 1 - if the file is not present
f = open('sample.txt','w')
f.write('Hello world')
f.close()
# since file is closed hence this will not work
f.write('hello')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-109-c02a4a856526> in <module>
      3 f.write('Hello world')
      4 f.close()
----> 5 f.write('hello')

ValueError: I/O operation on closed file.

# write multiline strings
f = open('sample1.txt','w')
f.write('hello world')
f.write('\nhow are you?')
f.close()
     

# case 2 - if the file is already present
f = open('sample.txt','w')
f.write('salman khan')
f.close()
     

# how exactly open() works?
     

# Problem with w mode
# introducing append mode
f = open('/content/sample1.txt','a')
f.write('\nI am fine')
f.close()
     

# write lines
L = ['hello\n','hi\n','how are you\n','I am fine']

f = open('/content/temp/sample.txt','w')
f.writelines(L)
f.close()
     

# reading from files
# -> using read()
f = open('/content/sample.txt','r')
s = f.read()
print(s)
f.close()
     
hello
hi
how are you
I am fine

# reading upto n chars
f = open('/content/sample.txt','r')
s = f.read(10)
print(s)
f.close()
     
hello
hi
h

# readline() -> to read line by line
f = open('/content/sample.txt','r')
print(f.readline(),end='')
print(f.readline(),end='')
f.close()
     
hello
hi

# reading entire using readline
f = open('/content/sample.txt','r')

while True:

  data = f.readline()

  if data == '':
    break
  else:
    print(data,end='')

f.close()
     
hello
hi
how are you
I am fine
Using Context Manager (With)
It's a good idea to close a file after usage as it will free up the resources
If we dont close it, garbage collector would close it
with keyword closes the file as soon as the usage is over

# with
with open('/content/sample1.txt','w') as f:
  f.write('selmon bhai')
     

f.write('hello')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-4-00cba062fa3d> in <module>
----> 1 f.write('hello')

ValueError: I/O operation on closed file.

# try f.read() now
with open('/content/sample.txt','r') as f:
  print(f.readline())
     
hello


# moving within a file -> 10 char then 10 char
with open('sample.txt','r') as f:
  print(f.read(10))
  print(f.read(10))
  print(f.read(10))
  print(f.read(10))
     
hello
hi
h
ow are you

I am fine


# benefit? -> to load a big file in memory
big_L = ['hello world ' for i in range(1000)]

with open('big.txt','w') as f:
  f.writelines(big_L)

     

with open('big.txt','r') as f:

  chunk_size = 10

  while len(f.read(chunk_size)) > 0:
    print(f.read(chunk_size),end='***')
    f.read(chunk_size)
     
d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***

# seek and tell function
with open('sample.txt','r') as f:
  f.seek(15)
  print(f.read(10))
  print(f.tell())

  print(f.read(10))
  print(f.tell())
     
e you
I am
25
 fine
30

# seek during write
with open('sample.txt','w') as f:
  f.write('Hello')
  f.seek(0)
  f.write('Xa')
     
Problems with working in text mode
can't work with binary files like images
not good for other data types like int/float/list/tuples

# working with binary file
with open('screenshot1.png','r') as f:
  f.read()
     
---------------------------------------------------------------------------
UnicodeDecodeError                        Traceback (most recent call last)
<ipython-input-23-b662b4ad1a91> in <module>
      1 # working with binary file
      2 with open('screenshot1.png','r') as f:
----> 3   f.read()

/usr/lib/python3.7/codecs.py in decode(self, input, final)
    320         # decode input (taking the buffer into account)
    321         data = self.buffer + input
--> 322         (result, consumed) = self._buffer_decode(data, self.errors, final)
    323         # keep undecoded input until the next call
    324         self.buffer = data[consumed:]

UnicodeDecodeError: 'utf-8' codec can't decode byte 0x89 in position 0: invalid start byte

# working with binary file
with open('screenshot1.png','rb') as f:
  with open('screenshot_copy.png','wb') as wf:
    wf.write(f.read())
     

# working with a big binary file
     

# working with other data types
with open('sample.txt','w') as f:
  f.write(5)
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-26-a8e7a73b1431> in <module>
      1 # working with other data types
      2 with open('sample.txt','w') as f:
----> 3   f.write(5)

TypeError: write() argument must be str, not int

with open('sample.txt','w') as f:
  f.write('5')
     

with open('sample.txt','r') as f:
  print(int(f.read()) + 5)
     
10


     

# more complex data
d = {
    'name':'nitish',
     'age':33,
     'gender':'male'
}

with open('sample.txt','w') as f:
  f.write(str(d))
     

with open('sample.txt','r') as f:
  print(dict(f.read()))
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-34-949b64f1fbe0> in <module>
      1 with open('sample.txt','r') as f:
----> 2   print(dict(f.read()))

ValueError: dictionary update sequence element #0 has length 1; 2 is required
Serialization and Deserialization
Serialization - process of converting python data types to JSON format
Deserialization - process of converting JSON to python data types
What is JSON?
image.png


# serialization using json module
# list
import json

L = [1,2,3,4]

with open('demo.json','w') as f:
  json.dump(L,f)

     

# dict
d = {
    'name':'nitish',
     'age':33,
     'gender':'male'
}

with open('demo.json','w') as f:
  json.dump(d,f,indent=4)
     

# deserialization
import json

with open('demo.json','r') as f:
  d = json.load(f)
  print(d)
  print(type(d))
     
{'name': 'nitish', 'age': 33, 'gender': 'male'}
<class 'dict'>

# serialize and deserialize tuple
import json

t = (1,2,3,4,5)

with open('demo.json','w') as f:
  json.dump(t,f)
     

# serialize and deserialize a nested dict

d = {
    'student':'nitish',
     'marks':[23,14,34,45,56]
}

with open('demo.json','w') as f:
  json.dump(d,f)
     
Serializing and Deserializing custom objects

class Person:

  def __init__(self,fname,lname,age,gender):
    self.fname = fname
    self.lname = lname
    self.age = age
    self.gender = gender

# format to printed in
# -> Nitish Singh age -> 33 gender -> male
     

person = Person('Nitish','Singh',33,'male')
     

# As a string
import json

def show_object(person):
  if isinstance(person,Person):
    return "{} {} age -> {} gender -> {}".format(person.fname,person.lname,person.age,person.gender)

with open('demo.json','w') as f:
  json.dump(person,f,default=show_object)
     

# As a dict
import json

def show_object(person):
  if isinstance(person,Person):
    return {'name':person.fname + ' ' + person.lname,'age':person.age,'gender':person.gender}

with open('demo.json','w') as f:
  json.dump(person,f,default=show_object,indent=4)
     

# indent arrtribute
# As a dict
     

# deserializing
import json

with open('demo.json','r') as f:
  d = json.load(f)
  print(d)
  print(type(d))
     
{'name': 'Nitish Singh', 'age': 33, 'gender': 'male'}
<class 'dict'>
Pickling
Pickling is the process whereby a Python object hierarchy is converted into a byte stream, and unpickling is the inverse operation, whereby a byte stream (from a binary file or bytes-like object) is converted back into an object hierarchy.


class Person:

  def __init__(self,name,age):
    self.name = name
    self.age = age

  def display_info(self):
    print('Hi my name is',self.name,'and I am ',self.age,'years old')
     

p = Person('nitish',33)

     

# pickle dump
import pickle
with open('person.pkl','wb') as f:
  pickle.dump(p,f)
     

# pickle load
import pickle
with open('person.pkl','rb') as f:
  p = pickle.load(f)

p.display_info()
     
Hi my name is nitish and I am  33 years old
Pickle Vs Json
Pickle lets the user to store data in binary format. JSON lets the user store data in a human-readable text format.


     


     
###################################################################################



Some Theory
Types of data used for I/O:
Text - '12345' as a sequence of unicode chars
Binary - 12345 as a sequence of bytes of its binary equivalent
Hence there are 2 file types to deal with
Text files - All program files are text files
Binary Files - Images,music,video,exe files
How File I/O is done in most programming languages
Open a file
Read/Write data
Close the file
Writing to a file

# case 1 - if the file is not present
f = open('sample.txt','w')
f.write('Hello world')
f.close()
# since file is closed hence this will not work
f.write('hello')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-109-c02a4a856526> in <module>
      3 f.write('Hello world')
      4 f.close()
----> 5 f.write('hello')

ValueError: I/O operation on closed file.

# write multiline strings
f = open('sample1.txt','w')
f.write('hello world')
f.write('\nhow are you?')
f.close()
     

# case 2 - if the file is already present
f = open('sample.txt','w')
f.write('salman khan')
f.close()
     

# how exactly open() works?
     

# Problem with w mode
# introducing append mode
f = open('/content/sample1.txt','a')
f.write('\nI am fine')
f.close()
     

# write lines
L = ['hello\n','hi\n','how are you\n','I am fine']

f = open('/content/temp/sample.txt','w')
f.writelines(L)
f.close()
     

# reading from files
# -> using read()
f = open('/content/sample.txt','r')
s = f.read()
print(s)
f.close()
     
hello
hi
how are you
I am fine

# reading upto n chars
f = open('/content/sample.txt','r')
s = f.read(10)
print(s)
f.close()
     
hello
hi
h

# readline() -> to read line by line
f = open('/content/sample.txt','r')
print(f.readline(),end='')
print(f.readline(),end='')
f.close()
     
hello
hi

# reading entire using readline
f = open('/content/sample.txt','r')

while True:

  data = f.readline()

  if data == '':
    break
  else:
    print(data,end='')

f.close()
     
hello
hi
how are you
I am fine
Using Context Manager (With)
It's a good idea to close a file after usage as it will free up the resources
If we dont close it, garbage collector would close it
with keyword closes the file as soon as the usage is over

# with
with open('/content/sample1.txt','w') as f:
  f.write('selmon bhai')
     

f.write('hello')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-4-00cba062fa3d> in <module>
----> 1 f.write('hello')

ValueError: I/O operation on closed file.

# try f.read() now
with open('/content/sample.txt','r') as f:
  print(f.readline())
     
hello


# moving within a file -> 10 char then 10 char
with open('sample.txt','r') as f:
  print(f.read(10))
  print(f.read(10))
  print(f.read(10))
  print(f.read(10))
     
hello
hi
h
ow are you

I am fine


# benefit? -> to load a big file in memory
big_L = ['hello world ' for i in range(1000)]

with open('big.txt','w') as f:
  f.writelines(big_L)

     

with open('big.txt','r') as f:

  chunk_size = 10

  while len(f.read(chunk_size)) > 0:
    print(f.read(chunk_size),end='***')
    f.read(chunk_size)
     
d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***d hello wo***o world he***

# seek and tell function
with open('sample.txt','r') as f:
  f.seek(15)
  print(f.read(10))
  print(f.tell())

  print(f.read(10))
  print(f.tell())
     
e you
I am
25
 fine
30

# seek during write
with open('sample.txt','w') as f:
  f.write('Hello')
  f.seek(0)
  f.write('Xa')
     
Problems with working in text mode
can't work with binary files like images
not good for other data types like int/float/list/tuples

# working with binary file
with open('screenshot1.png','r') as f:
  f.read()
     
---------------------------------------------------------------------------
UnicodeDecodeError                        Traceback (most recent call last)
<ipython-input-23-b662b4ad1a91> in <module>
      1 # working with binary file
      2 with open('screenshot1.png','r') as f:
----> 3   f.read()

/usr/lib/python3.7/codecs.py in decode(self, input, final)
    320         # decode input (taking the buffer into account)
    321         data = self.buffer + input
--> 322         (result, consumed) = self._buffer_decode(data, self.errors, final)
    323         # keep undecoded input until the next call
    324         self.buffer = data[consumed:]

UnicodeDecodeError: 'utf-8' codec can't decode byte 0x89 in position 0: invalid start byte

# working with binary file
with open('screenshot1.png','rb') as f:
  with open('screenshot_copy.png','wb') as wf:
    wf.write(f.read())
     

# working with a big binary file
     

# working with other data types
with open('sample.txt','w') as f:
  f.write(5)
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-26-a8e7a73b1431> in <module>
      1 # working with other data types
      2 with open('sample.txt','w') as f:
----> 3   f.write(5)

TypeError: write() argument must be str, not int

with open('sample.txt','w') as f:
  f.write('5')
     

with open('sample.txt','r') as f:
  print(int(f.read()) + 5)
     
10


     

# more complex data
d = {
    'name':'nitish',
     'age':33,
     'gender':'male'
}

with open('sample.txt','w') as f:
  f.write(str(d))
     

with open('sample.txt','r') as f:
  print(dict(f.read()))
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-34-949b64f1fbe0> in <module>
      1 with open('sample.txt','r') as f:
----> 2   print(dict(f.read()))

ValueError: dictionary update sequence element #0 has length 1; 2 is required
Serialization and Deserialization
Serialization - process of converting python data types to JSON format
Deserialization - process of converting JSON to python data types
What is JSON?
image.png


# serialization using json module
# list
import json

L = [1,2,3,4]

with open('demo.json','w') as f:
  json.dump(L,f)

     

# dict
d = {
    'name':'nitish',
     'age':33,
     'gender':'male'
}

with open('demo.json','w') as f:
  json.dump(d,f,indent=4)
     

# deserialization
import json

with open('demo.json','r') as f:
  d = json.load(f)
  print(d)
  print(type(d))
     
{'name': 'nitish', 'age': 33, 'gender': 'male'}
<class 'dict'>

# serialize and deserialize tuple
import json

t = (1,2,3,4,5)

with open('demo.json','w') as f:
  json.dump(t,f)
     

# serialize and deserialize a nested dict

d = {
    'student':'nitish',
     'marks':[23,14,34,45,56]
}

with open('demo.json','w') as f:
  json.dump(d,f)
     
Serializing and Deserializing custom objects

class Person:

  def __init__(self,fname,lname,age,gender):
    self.fname = fname
    self.lname = lname
    self.age = age
    self.gender = gender

# format to printed in
# -> Nitish Singh age -> 33 gender -> male
     

person = Person('Nitish','Singh',33,'male')
     

# As a string
import json

def show_object(person):
  if isinstance(person,Person):
    return "{} {} age -> {} gender -> {}".format(person.fname,person.lname,person.age,person.gender)

with open('demo.json','w') as f:
  json.dump(person,f,default=show_object)
     

# As a dict
import json

def show_object(person):
  if isinstance(person,Person):
    return {'name':person.fname + ' ' + person.lname,'age':person.age,'gender':person.gender}

with open('demo.json','w') as f:
  json.dump(person,f,default=show_object,indent=4)
     

# indent arrtribute
# As a dict
     

# deserializing
import json

with open('demo.json','r') as f:
  d = json.load(f)
  print(d)
  print(type(d))
     
{'name': 'Nitish Singh', 'age': 33, 'gender': 'male'}
<class 'dict'>
Pickling
Pickling is the process whereby a Python object hierarchy is converted into a byte stream, and unpickling is the inverse operation, whereby a byte stream (from a binary file or bytes-like object) is converted back into an object hierarchy.


class Person:

  def __init__(self,name,age):
    self.name = name
    self.age = age

  def display_info(self):
    print('Hi my name is',self.name,'and I am ',self.age,'years old')
     

p = Person('nitish',33)

     

# pickle dump
import pickle
with open('person.pkl','wb') as f:
  pickle.dump(p,f)
     

# pickle load
import pickle
with open('person.pkl','rb') as f:
  p = pickle.load(f)

p.display_info()
     
Hi my name is nitish and I am  33 years old
Pickle Vs Json
Pickle lets the user to store data in binary format. JSON lets the user store data in a human-readable text format.


     


     
###################################################################################



There are 2 stages where error may happen in a program

During compilation -> Syntax Error
During execution -> Exceptions
Syntax Error
Something in the program is not written according to the program grammar.
Error is raised by the interpreter/compiler
You can solve it by rectifying the program

# Examples of syntax error
print 'hello world'
     
  File "<ipython-input-3-4655b84ba7b7>", line 2
    print 'hello world'
                      ^
SyntaxError: Missing parentheses in call to 'print'. Did you mean print('hello world')?
Other examples of syntax error
Leaving symbols like colon,brackets
Misspelling a keyword
Incorrect indentation
empty if/else/loops/class/functions

a = 5
if a==3
  print('hello')
     
  File "<ipython-input-68-efc58c10458d>", line 2
    if a==3
           ^
SyntaxError: invalid syntax

a = 5
iff a==3:
  print('hello')
     
  File "<ipython-input-69-d1e6fae154d5>", line 2
    iff a==3:
        ^
SyntaxError: invalid syntax

a = 5
if a==3:
print('hello')
     
  File "<ipython-input-70-ccc702dc036c>", line 3
    print('hello')
        ^
IndentationError: expected an indented block

# IndexError
# The IndexError is thrown when trying to access an item at an invalid index.
L = [1,2,3]
L[100]
     
---------------------------------------------------------------------------
IndexError                                Traceback (most recent call last)
<ipython-input-71-c90668d2b194> in <module>
      2 # The IndexError is thrown when trying to access an item at an invalid index.
      3 L = [1,2,3]
----> 4 L[100]

IndexError: list index out of range

# ModuleNotFoundError
# The ModuleNotFoundError is thrown when a module could not be found.
import mathi
math.floor(5.3)
     
---------------------------------------------------------------------------
ModuleNotFoundError                       Traceback (most recent call last)
<ipython-input-73-cbdaf00191df> in <module>
      1 # ModuleNotFoundError
      2 # The ModuleNotFoundError is thrown when a module could not be found.
----> 3 import mathi
      4 math.floor(5.3)

ModuleNotFoundError: No module named 'mathi'

---------------------------------------------------------------------------
NOTE: If your import is failing due to a missing package, you can
manually install dependencies using either !pip or !apt.

To view examples of installing some common dependencies, click the
"Open Examples" button below.
---------------------------------------------------------------------------

# KeyError
# The KeyError is thrown when a key is not found

d = {'name':'nitish'}
d['age']
     
---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
<ipython-input-74-453afa1c9765> in <module>
      3 
      4 d = {'name':'nitish'}
----> 5 d['age']

KeyError: 'age'

# TypeError
# The TypeError is thrown when an operation or function is applied to an object of an inappropriate type.
1 + 'a'
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-78-2a3eb3f5bb0a> in <module>
      1 # TypeError
      2 # The TypeError is thrown when an operation or function is applied to an object of an inappropriate type.
----> 3 1 + 'a'

TypeError: unsupported operand type(s) for +: 'int' and 'str'

# ValueError
# The ValueError is thrown when a function's argument is of an inappropriate type.
int('a')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-76-e419d2a084b4> in <module>
      1 # ValueError
      2 # The ValueError is thrown when a function's argument is of an inappropriate type.
----> 3 int('a')

ValueError: invalid literal for int() with base 10: 'a'

# NameError
# The NameError is thrown when an object could not be found.
print(k)
     
---------------------------------------------------------------------------
NameError                                 Traceback (most recent call last)
<ipython-input-79-e3e8aaa4ec45> in <module>
      1 # NameError
      2 # The NameError is thrown when an object could not be found.
----> 3 print(k)

NameError: name 'k' is not defined

# AttributeError
L = [1,2,3]
L.upper()

# Stacktrace
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-80-dd5a29625ddc> in <module>
      1 # AttributeError
      2 L = [1,2,3]
----> 3 L.upper()

AttributeError: 'list' object has no attribute 'upper'
Exceptions
If things go wrong during the execution of the program(runtime). It generally happens when something unforeseen has happened.

Exceptions are raised by python runtime
You have to takle is on the fly
Examples
Memory overflow
Divide by 0 -> logical error
Database error

# Why is it important to handle exceptions
# how to handle exceptions
# -> Try except block
     

# let's create a file
with open('sample.txt','w') as f:
  f.write('hello world')
     

# try catch demo
try:
  with open('sample1.txt','r') as f:
    print(f.read())
except:
  print('sorry file not found')
     
sorry file not found

# catching specific exception
try:
  m=5
  f = open('sample1.txt','r')
  print(f.read())
  print(m)
  print(5/2)
  L = [1,2,3]
  L[100]
except FileNotFoundError:
  print('file not found')
except NameError:
  print('variable not defined')
except ZeroDivisionError:
  print("can't divide by 0")
except Exception as e:
  print(e)
     
[Errno 2] No such file or directory: 'sample1.txt'

# else
try:
  f = open('sample1.txt','r')
except FileNotFoundError:
  print('file nai mili')
except Exception:
  print('kuch to lafda hai')
else:
  print(f.read())


     
file nai mili

# finally
# else
try:
  f = open('sample1.txt','r')
except FileNotFoundError:
  print('file nai mili')
except Exception:
  print('kuch to lafda hai')
else:
  print(f.read())
finally:
  print('ye to print hoga hi')
     
file nai mili
ye to print hoga hi

# raise Exception
# In Python programming, exceptions are raised when errors occur at runtime.
# We can also manually raise exceptions using the raise keyword.

# We can optionally pass values to the exception to clarify why that exception was raised
     

raise ZeroDivisionError('aise hi try kar raha hu')
# Java
# try -> try
# except -> catch
# raise -> throw
     
---------------------------------------------------------------------------
ZeroDivisionError                         Traceback (most recent call last)
<ipython-input-106-5a07d7d89433> in <module>
----> 1 raise ZeroDivisionError('aise hi try kar raha hu')

ZeroDivisionError: aise hi try kar raha hu

class Bank:

  def __init__(self,balance):
    self.balance = balance

  def withdraw(self,amount):
    if amount < 0:
      raise Exception('amount cannot be -ve')
    if self.balance < amount:
      raise Exception('paise nai hai tere paas')
    self.balance = self.balance - amount

obj = Bank(10000)
try:
  obj.withdraw(15000)
except Exception as e:
  print(e)
else:
  print(obj.balance)
     
paise nai hai tere paas

class MyException(Exception):
  def __init__(self,message):
    print(message)

class Bank:

  def __init__(self,balance):
    self.balance = balance

  def withdraw(self,amount):
    if amount < 0:
      raise MyException('amount cannot be -ve')
    if self.balance < amount:
      raise MyException('paise nai hai tere paas')
    self.balance = self.balance - amount

obj = Bank(10000)
try:
  obj.withdraw(5000)
except MyException as e:
  pass
else:
  print(obj.balance)
     
5000



# creating custom exceptions
# exception hierarchy in python
     

# simple example
     

class SecurityError(Exception):

  def __init__(self,message):
    print(message)

  def logout(self):
    print('logout')

class Google:

  def __init__(self,name,email,password,device):
    self.name = name
    self.email = email
    self.password = password
    self.device = device

  def login(self,email,password,device):
    if device != self.device:
      raise SecurityError('bhai teri to lag gayi')
    if email == self.email and password == self.password:
      print('welcome')
    else:
      print('login error')



obj = Google('nitish','nitish@gmail.com','1234','android')

try:
  obj.login('nitish@gmail.com','1234','windows')
except SecurityError as e:
  e.logout()
else:
  print(obj.name)
finally:
  print('database connection closed')


     
bhai teri to lag gayi
logout
database connection closed


     
######################################################################################

months = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 
          'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
for month_idx, month in enumerate(months, start=1):
    print(f'{month_idx}: {month}')

1: jan
2: feb
3: mar
4: apr
5: may
6: jun
7: jul
8: aug
9: sep
10: oct
11: nov
12: dec




Namespaces
A namespace is a space that holds names(identifiers).Programmatically speaking, namespaces are dictionary of identifiers(keys) and their objects(values)

There are 4 types of namespaces:

Builtin Namespace
Global Namespace
Enclosing Namespace
Local Namespace
Scope and LEGB Rule
A scope is a textual region of a Python program where a namespace is directly accessible.

The interpreter searches for a name from the inside out, looking in the local, enclosing, global, and finally the built-in scope. If the interpreter doesn’t find the name in any of these locations, then Python raises a NameError exception.


# local and global
# global var
a = 2

def temp():
  # local var
  b = 3
  print(b)

temp()
print(a)
     
3
2

# local and global -> same name
a = 2

def temp():
  # local var
  a = 3
  print(b)

temp()
print(a)
     

# local and global -> local does not have but global has
a = 2

def temp():
  # local var
  print(a)

temp()
print(a)

     
2
2

# local and global -> editing global
a = 2

def temp():
  # local var
  a += 1
  print(a)

temp()
print(a)
     
---------------------------------------------------------------------------
UnboundLocalError                         Traceback (most recent call last)
<ipython-input-49-0bff4ae6448f> in <module>
      7   print(a)
      8 
----> 9 temp()
     10 print(a)

<ipython-input-49-0bff4ae6448f> in temp()
      4 def temp():
      5   # local var
----> 6   a += 1
      7   print(a)
      8 

UnboundLocalError: local variable 'a' referenced before assignment

a = 2

def temp():
  # local var
  global a
  a += 1
  print(a)

temp()
print(a)
     
3
3

# local and global -> global created inside local
def temp():
  # local var
  global a
  a = 1
  print(a)

temp()
print(a)
     

# local and global -> function parameter is local
def temp(z):
  # local var
  print(z)

a = 5
temp(5)
print(a)
print(z)
     
5
5
---------------------------------------------------------------------------
NameError                                 Traceback (most recent call last)
<ipython-input-51-aac3f4d9657f> in <module>
      7 temp(5)
      8 print(a)
----> 9 print(z)

NameError: name 'z' is not defined

# built-in scope
import builtins
print(dir(builtins))
     
['ArithmeticError', 'AssertionError', 'AttributeError', 'BaseException', 'BlockingIOError', 'BrokenPipeError', 'BufferError', 'BytesWarning', 'ChildProcessError', 'ConnectionAbortedError', 'ConnectionError', 'ConnectionRefusedError', 'ConnectionResetError', 'DeprecationWarning', 'EOFError', 'Ellipsis', 'EnvironmentError', 'Exception', 'False', 'FileExistsError', 'FileNotFoundError', 'FloatingPointError', 'FutureWarning', 'GeneratorExit', 'IOError', 'ImportError', 'ImportWarning', 'IndentationError', 'IndexError', 'InterruptedError', 'IsADirectoryError', 'KeyError', 'KeyboardInterrupt', 'LookupError', 'MemoryError', 'ModuleNotFoundError', 'NameError', 'None', 'NotADirectoryError', 'NotImplemented', 'NotImplementedError', 'OSError', 'OverflowError', 'PendingDeprecationWarning', 'PermissionError', 'ProcessLookupError', 'RecursionError', 'ReferenceError', 'ResourceWarning', 'RuntimeError', 'RuntimeWarning', 'StopAsyncIteration', 'StopIteration', 'SyntaxError', 'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError', 'TimeoutError', 'True', 'TypeError', 'UnboundLocalError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError', 'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning', 'ValueError', 'Warning', 'ZeroDivisionError', '__IPYTHON__', '__build_class__', '__debug__', '__doc__', '__import__', '__loader__', '__name__', '__package__', '__spec__', 'abs', 'all', 'any', 'ascii', 'bin', 'bool', 'breakpoint', 'bytearray', 'bytes', 'callable', 'chr', 'classmethod', 'compile', 'complex', 'copyright', 'credits', 'delattr', 'dict', 'dir', 'display', 'divmod', 'enumerate', 'eval', 'exec', 'execfile', 'filter', 'float', 'format', 'frozenset', 'get_ipython', 'getattr', 'globals', 'hasattr', 'hash', 'help', 'hex', 'id', 'input', 'int', 'isinstance', 'issubclass', 'iter', 'len', 'license', 'list', 'locals', 'map', 'max', 'memoryview', 'min', 'next', 'object', 'oct', 'open', 'ord', 'pow', 'print', 'property', 'range', 'repr', 'reversed', 'round', 'runfile', 'set', 'setattr', 'slice', 'sorted', 'staticmethod', 'str', 'sum', 'super', 'tuple', 'type', 'vars', 'zip']

# how to see all the built-ins
     

# renaming built-ins
L = [1,2,3]
print(max(L))
def max():
  print('hello')

print(max(L))
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-68-c19f3451a38f> in <module>
      1 # renaming built-ins
      2 L = [1,2,3]
----> 3 print(max(L))
      4 def max():
      5   print('hello')

TypeError: max() takes 0 positional arguments but 1 was given

# Enclosing scope
def outer():
  def inner():
    print(a)
  inner()
  print('outer function')


outer()
print('main program')
     
1
outer function
main program

# nonlocal keyword
def outer():
  a = 1
  def inner():
    nonlocal a
    a += 1
    print('inner',a)
  inner()
  print('outer',a)


outer()
print('main program')
     
inner 2
outer 2
main program

# Summary
     
Decorators
A decorator in python is a function that receives another function as input and adds some functionality(decoration) to and it and returns it.

This can happen only because python functions are 1st class citizens.

There are 2 types of decorators available in python

Built in decorators like @staticmethod, @classmethod, @abstractmethod and @property etc
User defined decorators that we programmers can create according to our needs

# Python are 1st class function

def modify(func,num):
  return func(num)

def square(num):
  return num**2

modify(square,2)
     
4

# simple example

def my_decorator(func):
  def wrapper():
    print('***********************')
    func()
    print('***********************')
  return wrapper

def hello():
  print('hello')

def display():
  print('hello nitish')

a = my_decorator(hello)
a()

b = my_decorator(display)
b()
     
***********************
hello
***********************
***********************
hello nitish
***********************

# more functions
     

# how this works -> closure?
     

# python tutor
     

# Better syntax?
# simple example

def my_decorator(func):
  def wrapper():
    print('***********************')
    func()
    print('***********************')
  return wrapper

@my_decorator
def hello():
  print('hello')

hello()
     
***********************
hello
***********************

# anything meaningful?
import time

def timer(func):
  def wrapper(*args):
    start = time.time()
    func(*args)
    print('time taken by',func.__name__,time.time()-start,'secs')
  return wrapper

@timer
def hello():
  print('hello wolrd')
  time.sleep(2)

@timer
def square(num):
  time.sleep(1)
  print(num**2)

@timer
def power(a,b):
  print(a**b)

hello()
square(2)
power(2,3)

     
hello wolrd
time taken by hello 2.003671884536743 secs
4
time taken by square 1.0009939670562744 secs
8
time taken by power 2.1696090698242188e-05 secs

# A big problem
     

# One last example -> decorators with arguments

     

@checkdt(int)
def square(num):
  print(num**2)
     

def sanity_check(data_type):
  def outer_wrapper(func):
    def inner_wrapper(*args):
      if type(*args) == data_type:
        func(*args)
      else:
        raise TypeError('Ye datatype nai chalega')
    return inner_wrapper
  return outer_wrapper

@sanity_check(int)
def square(num):
  print(num**2)

@sanity_check(str)
def greet(name):
  print('hello',name)

square(2)
     
4


     


     
##################################################################################







What is numpy?
NumPy is the fundamental package for scientific computing in Python. It is a Python library that provides a multidimensional array object, various derived objects (such as masked arrays and matrices), and an assortment of routines for fast operations on arrays, including mathematical, logical, shape manipulation, sorting, selecting, I/O, discrete Fourier transforms, basic linear algebra, basic statistical operations, random simulation and much more.

At the core of the NumPy package, is the ndarray object. This encapsulates n-dimensional arrays of homogeneous data types

Numpy Arrays Vs Python Sequences
NumPy arrays have a fixed size at creation, unlike Python lists (which can grow dynamically). Changing the size of an ndarray will create a new array and delete the original.

The elements in a NumPy array are all required to be of the same data type, and thus will be the same size in memory.

NumPy arrays facilitate advanced mathematical and other types of operations on large numbers of data. Typically, such operations are executed more efficiently and with less code than is possible using Python’s built-in sequences.

A growing plethora of scientific and mathematical Python-based packages are using NumPy arrays; though these typically support Python-sequence input, they convert such input to NumPy arrays prior to processing, and they often output NumPy arrays.

Creating Numpy Arrays

# np.array
import numpy as np

a = np.array([1,2,3])
print(a)
     
[1 2 3]

# 2D and 3D
b = np.array([[1,2,3],[4,5,6]])
print(b)
     
[[1 2 3]
 [4 5 6]]

c = np.array([[[1,2],[3,4]],[[5,6],[7,8]]])
print(c)
     
[[[1 2]
  [3 4]]

 [[5 6]
  [7 8]]]

# dtype
np.array([1,2,3],dtype=float)
     
array([1., 2., 3.])

# np.arange
np.arange(1,11,2)
     
array([1, 3, 5, 7, 9])

# with reshape
np.arange(16).reshape(2,2,2,2)
     
array([[[[ 0,  1],
         [ 2,  3]],

        [[ 4,  5],
         [ 6,  7]]],


       [[[ 8,  9],
         [10, 11]],

        [[12, 13],
         [14, 15]]]])

# np.ones and np.zeros
np.ones((3,4))
     
array([[1., 1., 1., 1.],
       [1., 1., 1., 1.],
       [1., 1., 1., 1.]])

np.zeros((3,4))
     
array([[0., 0., 0., 0.],
       [0., 0., 0., 0.],
       [0., 0., 0., 0.]])

# np.random
np.random.random((3,4))
     
array([[0.85721156, 0.31248316, 0.08807828, 0.35230774],
       [0.96813914, 0.44681708, 0.56396358, 0.53020065],
       [0.03277116, 0.28543753, 0.09521082, 0.87967034]])

# np.linspace
np.linspace(-10,10,10,dtype=int)
     
array([-10,  -8,  -6,  -4,  -2,   1,   3,   5,   7,  10])

# np.identity
np.identity(3)
     
array([[1., 0., 0.],
       [0., 1., 0.],
       [0., 0., 1.]])
Array Attributes

a1 = np.arange(10,dtype=np.int32)
a2 = np.arange(12,dtype=float).reshape(3,4)
a3 = np.arange(8).reshape(2,2,2)

a3
     
array([[[0, 1],
        [2, 3]],

       [[4, 5],
        [6, 7]]])

# ndim
a3.ndim
     
3

# shape
print(a3.shape)
a3
     
(2, 2, 2)
array([[[0, 1],
        [2, 3]],

       [[4, 5],
        [6, 7]]])

# size
print(a2.size)
a2
     
12
array([[ 0.,  1.,  2.,  3.],
       [ 4.,  5.,  6.,  7.],
       [ 8.,  9., 10., 11.]])

# itemsize
a3.itemsize
     
8

# dtype
print(a1.dtype)
print(a2.dtype)
print(a3.dtype)


     
int32
float64
int64
Changing Datatype

# astype
a3.astype(np.int32)
     
array([[[0, 1],
        [2, 3]],

       [[4, 5],
        [6, 7]]], dtype=int32)
Array Operations

a1 = np.arange(12).reshape(3,4)
a2 = np.arange(12,24).reshape(3,4)

a2
     
array([[12, 13, 14, 15],
       [16, 17, 18, 19],
       [20, 21, 22, 23]])

# scalar operations

# arithmetic
a1 ** 2
     
array([[  0,   1,   4,   9],
       [ 16,  25,  36,  49],
       [ 64,  81, 100, 121]])

# relational
a2 == 15
     
array([[False, False, False,  True],
       [False, False, False, False],
       [False, False, False, False]])

# vector operations
# arithmetic
a1 ** a2
     
array([[                   0,                    1,                16384,
                    14348907],
       [          4294967296,         762939453125,      101559956668416,
           11398895185373143],
       [ 1152921504606846976, -1261475310744950487,  1864712049423024128,
         6839173302027254275]])
Array Functions

a1 = np.random.random((3,3))
a1 = np.round(a1*100)
a1
     
array([[43., 28., 71.],
       [27., 93., 36.],
       [31., 18.,  7.]])

# max/min/sum/prod
# 0 -> col and 1 -> row
np.prod(a1,axis=0)
     
array([35991., 46872., 17892.])

# mean/median/std/var
np.var(a1,axis=1)
     
array([317.55555556, 854.        ,  96.22222222])

# trigonomoetric functions
np.sin(a1)
     
array([[-0.83177474,  0.27090579,  0.95105465],
       [ 0.95637593, -0.94828214, -0.99177885],
       [-0.40403765, -0.75098725,  0.6569866 ]])

# dot product
a2 = np.arange(12).reshape(3,4)
a3 = np.arange(12,24).reshape(4,3)

np.dot(a2,a3)
     
array([[114, 120, 126],
       [378, 400, 422],
       [642, 680, 718]])

# log and exponents
np.exp(a1)
     
array([[4.72783947e+18, 1.44625706e+12, 6.83767123e+30],
       [5.32048241e+11, 2.45124554e+40, 4.31123155e+15],
       [2.90488497e+13, 6.56599691e+07, 1.09663316e+03]])

# round/floor/ceil

np.ceil(np.random.random((2,3))*100)
     
array([[48.,  4.,  6.],
       [ 3., 18., 82.]])
Indexing and Slicing

a1 = np.arange(10)
a2 = np.arange(12).reshape(3,4)
a3 = np.arange(8).reshape(2,2,2)

a3
     
array([[[0, 1],
        [2, 3]],

       [[4, 5],
        [6, 7]]])

a1
     
array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

a2
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11]])

a2[1,0]
     
4

a3
     
array([[[0, 1],
        [2, 3]],

       [[4, 5],
        [6, 7]]])

a3[1,0,1]
     
5

a3[1,1,0]
     
6

a1
     
array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

a1[2:5:2]
     
array([2, 4])

a2
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11]])

a2[0:2,1::2]
     
array([[1, 3],
       [5, 7]])

a2[::2,1::2]
     
array([[ 1,  3],
       [ 9, 11]])

a2[1,::3]
     
array([4, 7])

a2[0,:]
     
array([0, 1, 2, 3])

a2[:,2]
     
array([ 2,  6, 10])

a2[1:,1:3]
     
array([[ 5,  6],
       [ 9, 10]])

a3 = np.arange(27).reshape(3,3,3)
a3
     
array([[[ 0,  1,  2],
        [ 3,  4,  5],
        [ 6,  7,  8]],

       [[ 9, 10, 11],
        [12, 13, 14],
        [15, 16, 17]],

       [[18, 19, 20],
        [21, 22, 23],
        [24, 25, 26]]])

a3[::2,0,::2]
     
array([[ 0,  2],
       [18, 20]])

a3[2,1:,1:]
     
array([[22, 23],
       [25, 26]])

a3[0,1,:]
     
array([3, 4, 5])


     


     


     


     


     
Iterating

a1

for i in a1:
  print(i)
     
0
1
2
3
4
5
6
7
8
9

a2
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11]])

for i in a2:
  print(i)
     
[0 1 2 3]
[4 5 6 7]
[ 8  9 10 11]

a3
     
array([[[ 0,  1,  2],
        [ 3,  4,  5],
        [ 6,  7,  8]],

       [[ 9, 10, 11],
        [12, 13, 14],
        [15, 16, 17]],

       [[18, 19, 20],
        [21, 22, 23],
        [24, 25, 26]]])

for i in a3:
  print(i)
     
[[0 1 2]
 [3 4 5]
 [6 7 8]]
[[ 9 10 11]
 [12 13 14]
 [15 16 17]]
[[18 19 20]
 [21 22 23]
 [24 25 26]]

for i in np.nditer(a3):
  print(i)
     
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
Reshaping

# reshape
     

# Transpose
np.transpose(a2)
a2.T
     
array([[ 0,  4,  8],
       [ 1,  5,  9],
       [ 2,  6, 10],
       [ 3,  7, 11]])

# ravel
a3.ravel()
     
array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26])
Stacking

# horizontal stacking
a4 = np.arange(12).reshape(3,4)
a5 = np.arange(12,24).reshape(3,4)
a5
     
array([[12, 13, 14, 15],
       [16, 17, 18, 19],
       [20, 21, 22, 23]])

np.hstack((a4,a5))
     
array([[ 0,  1,  2,  3, 12, 13, 14, 15],
       [ 4,  5,  6,  7, 16, 17, 18, 19],
       [ 8,  9, 10, 11, 20, 21, 22, 23]])

# Vertical stacking
np.vstack((a4,a5))
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11],
       [12, 13, 14, 15],
       [16, 17, 18, 19],
       [20, 21, 22, 23]])
Splitting

# horizontal splitting
a4
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11]])

np.hsplit(a4,5)
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-227-59485ca7f23c> in <module>
----> 1 np.hsplit(a4,5)

<__array_function__ internals> in hsplit(*args, **kwargs)

/usr/local/lib/python3.8/dist-packages/numpy/lib/shape_base.py in hsplit(ary, indices_or_sections)
    938         raise ValueError('hsplit only works on arrays of 1 or more dimensions')
    939     if ary.ndim > 1:
--> 940         return split(ary, indices_or_sections, 1)
    941     else:
    942         return split(ary, indices_or_sections, 0)

<__array_function__ internals> in split(*args, **kwargs)

/usr/local/lib/python3.8/dist-packages/numpy/lib/shape_base.py in split(ary, indices_or_sections, axis)
    870         N = ary.shape[axis]
    871         if N % sections:
--> 872             raise ValueError(
    873                 'array split does not result in an equal division') from None
    874     return array_split(ary, indices_or_sections, axis)

ValueError: array split does not result in an equal division

# vertical splitting
     

a5
     
array([[12, 13, 14, 15],
       [16, 17, 18, 19],
       [20, 21, 22, 23]])

np.vsplit(a5,2)
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-230-5b73f701499e> in <module>
----> 1 np.vsplit(a5,2)

<__array_function__ internals> in vsplit(*args, **kwargs)

/usr/local/lib/python3.8/dist-packages/numpy/lib/shape_base.py in vsplit(ary, indices_or_sections)
    989     if _nx.ndim(ary) < 2:
    990         raise ValueError('vsplit only works on arrays of 2 or more dimensions')
--> 991     return split(ary, indices_or_sections, 0)
    992 
    993 

<__array_function__ internals> in split(*args, **kwargs)

/usr/local/lib/python3.8/dist-packages/numpy/lib/shape_base.py in split(ary, indices_or_sections, axis)
    870         N = ary.shape[axis]
    871         if N % sections:
--> 872             raise ValueError(
    873                 'array split does not result in an equal division') from None
    874     return array_split(ary, indices_or_sections, axis)

ValueError: array split does not result in an equal division


     
##################################################################################




Numpy array vs Python lists

# speed
# list
a = [i for i in range(10000000)]
b = [i for i in range(10000000,20000000)]

c = []
import time

start = time.time()
for i in range(len(a)):
  c.append(a[i] + b[i])
print(time.time()-start)
     
3.2699835300445557

# numpy
import numpy as np
a = np.arange(10000000)
b = np.arange(10000000,20000000)

start = time.time()
c = a + b
print(time.time()-start)
     
0.06481003761291504

3.26/0.06
     
54.33333333333333

# memory
a = [i for i in range(10000000)]
import sys

sys.getsizeof(a)
     
81528048

a = np.arange(10000000,dtype=np.int8)
sys.getsizeof(a)
     
10000104

# convenience
     
Advanced Indexing

# Normal Indexing and slicing

a = np.arange(24).reshape(6,4)
a
     
array([[ 0,  1,  2,  3],
       [ 4,  5,  6,  7],
       [ 8,  9, 10, 11],
       [12, 13, 14, 15],
       [16, 17, 18, 19],
       [20, 21, 22, 23]])

a[1,2]
     
5

a[1:3,1:3]
     
array([[4, 5],
       [7, 8]])

# Fancy Indexing

a[:,[0,2,3]]
     
array([[ 0,  2,  3],
       [ 4,  6,  7],
       [ 8, 10, 11],
       [12, 14, 15],
       [16, 18, 19],
       [20, 22, 23]])


     

# Boolean Indexing
a = np.random.randint(1,100,24).reshape(6,4)
a
     
array([[76, 98, 99, 39],
       [91, 46, 88, 23],
       [45,  6, 83,  1],
       [37, 43, 78, 85],
       [54, 73, 61, 53],
       [40, 93, 85, 77]])

# find all numbers greater than 50
a[a > 50]
     
array([76, 98, 99, 91, 88, 83, 78, 85, 54, 73, 61, 53, 93, 85, 77])

# find out even numbers
a[a % 2 == 0]
     
array([76, 98, 46, 88,  6, 78, 54, 40])

# find all numbers greater than 50 and are even

a[(a > 50) & (a % 2 == 0)]
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-97-0e69559201d8> in <module>
      1 # find all numbers greater than 50 and are even
      2 
----> 3 a[(a > 50) and (a % 2 == 0)]

ValueError: The truth value of an array with more than one element is ambiguous. Use a.any() or a.all()

# find all numbers not divisible by 7
a[~(a % 7 == 0)]
     
array([76, 99, 39, 46, 88, 23, 45,  6, 83,  1, 37, 43, 78, 85, 54, 73, 61,
       53, 40, 93, 85])
Broadcasting
The term broadcasting describes how NumPy treats arrays with different shapes during arithmetic operations.

The smaller array is “broadcast” across the larger array so that they have compatible shapes.


# same shape
a = np.arange(6).reshape(2,3)
b = np.arange(6,12).reshape(2,3)

print(a)
print(b)

print(a+b)
     
[[0 1 2]
 [3 4 5]]
[[ 6  7  8]
 [ 9 10 11]]
[[ 6  8 10]
 [12 14 16]]

# diff shape
a = np.arange(6).reshape(2,3)
b = np.arange(3).reshape(1,3)

print(a)
print(b)

print(a+b)
     
[[0 1 2]
 [3 4 5]]
[[0 1 2]]
[[0 2 4]
 [3 5 7]]
Broadcasting Rules
1. Make the two arrays have the same number of dimensions.

If the numbers of dimensions of the two arrays are different, add new dimensions with size 1 to the head of the array with the smaller dimension.
2. Make each dimension of the two arrays the same size.

If the sizes of each dimension of the two arrays do not match, dimensions with size 1 are stretched to the size of the other array.
If there is a dimension whose size is not 1 in either of the two arrays, it cannot be broadcasted, and an error is raised.


# More examples

a = np.arange(12).reshape(4,3)
b = np.arange(3)

print(a)
print(b)

print(a+b)
     
[[ 0  1  2]
 [ 3  4  5]
 [ 6  7  8]
 [ 9 10 11]]
[0 1 2]
[[ 0  2  4]
 [ 3  5  7]
 [ 6  8 10]
 [ 9 11 13]]

a = np.arange(12).reshape(3,4)
b = np.arange(3)

print(a)
print(b)

print(a+b)
     
[[ 0  1  2  3]
 [ 4  5  6  7]
 [ 8  9 10 11]]
[0 1 2]
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-104-fa6cbb589166> in <module>
      5 print(b)
      6 
----> 7 print(a+b)

ValueError: operands could not be broadcast together with shapes (3,4) (3,) 

a = np.arange(3).reshape(1,3)
b = np.arange(3).reshape(3,1)

print(a)
print(b)

print(a+b)
     
[[0 1 2]]
[[0]
 [1]
 [2]]
[[0 1 2]
 [1 2 3]
 [2 3 4]]

a = np.arange(3).reshape(1,3)
b = np.arange(4).reshape(4,1)

print(a)
print(b)

print(a + b)
     
[[0 1 2]]
[[0]
 [1]
 [2]
 [3]]
[[0 1 2]
 [1 2 3]
 [2 3 4]
 [3 4 5]]

a = np.array([1])
# shape -> (1,1)
b = np.arange(4).reshape(2,2)
# shape -> (2,2)

print(a)
print(b)

print(a+b)
     
[1]
[[0 1]
 [2 3]]
[[1 2]
 [3 4]]

a = np.arange(12).reshape(3,4)
b = np.arange(12).reshape(4,3)

print(a)
print(b)

print(a+b)
     
[[ 0  1  2  3]
 [ 4  5  6  7]
 [ 8  9 10 11]]
[[ 0  1  2]
 [ 3  4  5]
 [ 6  7  8]
 [ 9 10 11]]
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-109-c590a65467e5> in <module>
      5 print(b)
      6 
----> 7 print(a+b)

ValueError: operands could not be broadcast together with shapes (3,4) (4,3) 

a = np.arange(16).reshape(4,4)
b = np.arange(4).reshape(2,2)

print(a)
print(b)

print(a+b)
     
[[ 0  1  2  3]
 [ 4  5  6  7]
 [ 8  9 10 11]
 [12 13 14 15]]
[[0 1]
 [2 3]]
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-110-57df50a0058a> in <module>
      5 print(b)
      6 
----> 7 print(a+b)

ValueError: operands could not be broadcast together with shapes (4,4) (2,2) 
Working with mathematical formulas

a = np.arange(10)
np.sin(a)
     
array([ 0.        ,  0.84147098,  0.90929743,  0.14112001, -0.7568025 ,
       -0.95892427, -0.2794155 ,  0.6569866 ,  0.98935825,  0.41211849])

# sigmoid
def sigmoid(array):
  return 1/(1 + np.exp(-(array)))


a = np.arange(100)

sigmoid(a)
     
array([0.5       , 0.73105858, 0.88079708, 0.95257413, 0.98201379,
       0.99330715, 0.99752738, 0.99908895, 0.99966465, 0.99987661,
       0.9999546 , 0.9999833 , 0.99999386, 0.99999774, 0.99999917,
       0.99999969, 0.99999989, 0.99999996, 0.99999998, 0.99999999,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ])

# mean squared error

actual = np.random.randint(1,50,25)
predicted = np.random.randint(1,50,25)
     

def mse(actual,predicted):
  return np.mean((actual - predicted)**2)

mse(actual,predicted)
     
500.12

# binary cross entropy
np.mean((actual - predicted)**2)
     
500.12

actual
     
array([ 5,  3,  9,  7,  3, 36, 49, 28, 20, 40,  2, 23, 29, 18, 30, 23,  7,
       40, 15, 11, 27, 44, 32, 28, 10])
Working with missing values

# Working with missing values -> np.nan
a = np.array([1,2,3,4,np.nan,6])
a
     
array([ 1.,  2.,  3.,  4., nan,  6.])

a[~np.isnan(a)]
     
array([1., 2., 3., 4., 6.])
Plotting Graphs

# plotting a 2D plot
# x = y
import matplotlib.pyplot as plt

x = np.linspace(-10,10,100)
y = x

plt.plot(x,y)
     
[<matplotlib.lines.Line2D at 0x7f6f78e18f70>]


# y = x^2
x = np.linspace(-10,10,100)
y = x**2

plt.plot(x,y)
     
[<matplotlib.lines.Line2D at 0x7f6f87acf100>]


# y = sin(x)
x = np.linspace(-10,10,100)
y = np.sin(x)

plt.plot(x,y)
     
[<matplotlib.lines.Line2D at 0x7f6f5d1d0100>]


# y = xlog(x)
x = np.linspace(-10,10,100)
y = x * np.log(x)

plt.plot(x,y)
     
<ipython-input-137-4b3958c08378>:3: RuntimeWarning: invalid value encountered in log
  y = x * np.log(x)
[<matplotlib.lines.Line2D at 0x7f6f57ab62e0>]


# sigmoid
x = np.linspace(-10,10,100)
y = 1/(1+np.exp(-x))

plt.plot(x,y)
     
[<matplotlib.lines.Line2D at 0x7f6f5401e100>]

Meshgrids

# Meshgrids
     


     
####################################################################################




np.sort
Return a sorted copy of an array.

https://numpy.org/doc/stable/reference/generated/numpy.sort.html


# code
import numpy as np
a = np.random.randint(1,100,15)
a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

b = np.random.randint(1,100,24).reshape(6,4)
b
     
array([[12, 52, 42,  6],
       [29, 18, 47, 55],
       [61, 93, 83,  9],
       [38, 63, 44, 85],
       [ 8, 87, 31, 72],
       [40, 71,  2,  7]])

np.sort(a)[::-1]
     
array([94, 92, 78, 68, 53, 50, 38, 37, 30, 28, 21, 11,  9,  5,  2])

np.sort(b,axis=0)
     
array([[ 8, 18,  2,  6],
       [12, 52, 31,  7],
       [29, 63, 42,  9],
       [38, 71, 44, 55],
       [40, 87, 47, 72],
       [61, 93, 83, 85]])
np.append
The numpy.append() appends values along the mentioned axis at the end of the array

https://numpy.org/doc/stable/reference/generated/numpy.append.html


# code
np.append(a,200)
     
array([ 11,  53,  28,  50,  38,  37,  94,  92,   5,  30,  68,   9,  78,
         2,  21, 200])

b
     
array([[12, 52, 42,  6],
       [29, 18, 47, 55],
       [61, 93, 83,  9],
       [38, 63, 44, 85],
       [ 8, 87, 31, 72],
       [40, 71,  2,  7]])

np.append(b,np.random.random((b.shape[0],1)),axis=1)
     
array([[12.        , 52.        , 42.        ,  6.        ,  0.22006275],
       [29.        , 18.        , 47.        , 55.        ,  0.81740634],
       [61.        , 93.        , 83.        ,  9.        ,  0.89146072],
       [38.        , 63.        , 44.        , 85.        ,  0.84519124],
       [ 8.        , 87.        , 31.        , 72.        ,  0.24007274],
       [40.        , 71.        ,  2.        ,  7.        ,  0.48056374]])
np.concatenate
numpy.concatenate() function concatenate a sequence of arrays along an existing axis.

https://numpy.org/doc/stable/reference/generated/numpy.concatenate.html


# code
c = np.arange(6).reshape(2,3)
d = np.arange(6,12).reshape(2,3)

print(c)
print(d)
     
[[0 1 2]
 [3 4 5]]
[[ 6  7  8]
 [ 9 10 11]]

np.concatenate((c,d),axis=0)
     
array([[ 0,  1,  2],
       [ 3,  4,  5],
       [ 6,  7,  8],
       [ 9, 10, 11]])

np.concatenate((c,d),axis=1)
     
array([[ 0,  1,  2,  6,  7,  8],
       [ 3,  4,  5,  9, 10, 11]])
np.unique
With the help of np.unique() method, we can get the unique values from an array given as parameter in np.unique() method.

https://numpy.org/doc/stable/reference/generated/numpy.unique.html/


# code
e = np.array([1,1,2,2,3,3,4,4,5,5,6,6])
     

np.unique(e)
     
array([1, 2, 3, 4, 5, 6])
np.expand_dims
With the help of Numpy.expand_dims() method, we can get the expanded dimensions of an array

https://numpy.org/doc/stable/reference/generated/numpy.expand_dims.html


# code
a.shape
     
(15,)

np.expand_dims(a,axis=0).shape
     
(1, 15)

np.expand_dims(a,axis=1)
     
array([[11],
       [53],
       [28],
       [50],
       [38],
       [37],
       [94],
       [92],
       [ 5],
       [30],
       [68],
       [ 9],
       [78],
       [ 2],
       [21]])
np.where
The numpy.where() function returns the indices of elements in an input array where the given condition is satisfied.

https://numpy.org/doc/stable/reference/generated/numpy.where.html


a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

# find all indices with value greater than 50
np.where(a>50)
     
(array([ 1,  6,  7, 10, 12]),)

# replace all values > 50 with 0
np.where(a>50,0,a)
     
array([11,  0, 28, 50, 38, 37,  0,  0,  5, 30,  0,  9,  0,  2, 21])

np.where(a%2 == 0,0,a)
     
array([11, 53,  0,  0,  0, 37,  0,  0,  5,  0,  0,  9,  0,  0, 21])
np.argmax
The numpy.argmax() function returns indices of the max element of the array in a particular axis.

https://numpy.org/doc/stable/reference/generated/numpy.argmax.html


# code
a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

np.argmax(a)
     
6

b
     
array([[12, 52, 42,  6],
       [29, 18, 47, 55],
       [61, 93, 83,  9],
       [38, 63, 44, 85],
       [ 8, 87, 31, 72],
       [40, 71,  2,  7]])

np.argmax(b,axis=0)
     
array([2, 2, 2, 3])

np.argmax(b,axis=1)
     
array([1, 3, 1, 3, 1, 1])

# np.argmin
np.argmin(a)
     
13
np.cumsum
numpy.cumsum() function is used when we want to compute the cumulative sum of array elements over a given axis.

https://numpy.org/doc/stable/reference/generated/numpy.cumsum.html


a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

np.cumsum(a)
     
array([ 11,  64,  92, 142, 180, 217, 311, 403, 408, 438, 506, 515, 593,
       595, 616])

b
     
array([[12, 52, 42,  6],
       [29, 18, 47, 55],
       [61, 93, 83,  9],
       [38, 63, 44, 85],
       [ 8, 87, 31, 72],
       [40, 71,  2,  7]])

np.cumsum(b,axis=1)
     
array([[ 12,  64, 106, 112],
       [ 29,  47,  94, 149],
       [ 61, 154, 237, 246],
       [ 38, 101, 145, 230],
       [  8,  95, 126, 198],
       [ 40, 111, 113, 120]])

np.cumsum(b)
     
array([  12,   64,  106,  112,  141,  159,  206,  261,  322,  415,  498,
        507,  545,  608,  652,  737,  745,  832,  863,  935,  975, 1046,
       1048, 1055])

# np.cumprod
np.cumprod(a)
     
array([                  11,                  583,                16324,
                     816200,             31015600,           1147577200,
               107872256800,        9924247625600,       49621238128000,
           1488637143840000,   101227325781120000,   911045932030080000,
       -2725393596491966464, -5450787192983932928, -3786066610405281792])

a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])
np.percentile
numpy.percentile()function used to compute the nth percentile of the given data (array elements) along the specified axis.

https://numpy.org/doc/stable/reference/generated/numpy.percentile.html


a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

np.percentile(a,50)
     
37.0

np.median(a)
     
37.0
np.histogram
Numpy has a built-in numpy.histogram() function which represents the frequency of data distribution in the graphical form.

https://numpy.org/doc/stable/reference/generated/numpy.histogram.html


# code
a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

np.histogram(a,bins=[0,50,100])
     
(array([9, 6]), array([  0,  50, 100]))
np.corrcoef
Return Pearson product-moment correlation coefficients.

https://numpy.org/doc/stable/reference/generated/numpy.corrcoef.html


salary = np.array([20000,40000,25000,35000,60000])
experience = np.array([1,3,2,4,2])

np.corrcoef(salary,experience)
     
array([[1.        , 0.25344572],
       [0.25344572, 1.        ]])
np.isin
With the help of numpy.isin() method, we can see that one array having values are checked in a different numpy array having different elements with different sizes.

https://numpy.org/doc/stable/reference/generated/numpy.isin.html


# code
a

     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

items = [10,20,30,40,50,60,70,80,90,100]

a[np.isin(a,items)]
     
array([50, 30])
np.flip
The numpy.flip() function reverses the order of array elements along the specified axis, preserving the shape of the array.

https://numpy.org/doc/stable/reference/generated/numpy.flip.html


# code
a
     
array([11, 53, 28, 50, 38, 37, 94, 92,  5, 30, 68,  9, 78,  2, 21])

np.flip(a)
     
array([21,  2, 78,  9, 68, 30,  5, 92, 94, 37, 38, 50, 28, 53, 11])

b
     
array([[12, 52, 42,  6],
       [29, 18, 47, 55],
       [61, 93, 83,  9],
       [38, 63, 44, 85],
       [ 8, 87, 31, 72],
       [40, 71,  2,  7]])

np.flip(b,axis=1)
     
array([[ 6, 42, 52, 12],
       [55, 47, 18, 29],
       [ 9, 83, 93, 61],
       [85, 44, 63, 38],
       [72, 31, 87,  8],
       [ 7,  2, 71, 40]])
np.put
The numpy.put() function replaces specific elements of an array with given values of p_array. Array indexed works on flattened array.

https://numpy.org/doc/stable/reference/generated/numpy.put.html


# code
a
     
array([110, 530,  28,  50,  38,  37,  94,  92,   5,  30,  68,   9,  78,
         2,  21])

np.put(a,[0,1],[110,530])
     
np.delete
The numpy.delete() function returns a new array with the deletion of sub-arrays along with the mentioned axis.

https://numpy.org/doc/stable/reference/generated/numpy.delete.html


# code
a
     
array([110, 530,  28,  50,  38,  37,  94,  92,   5,  30,  68,   9,  78,
         2,  21])

np.delete(a,[0,2,4])
     
array([530,  50,  37,  94,  92,   5,  30,  68,   9,  78,   2,  21])
Set functions
np.union1d
np.intersect1d
np.setdiff1d
np.setxor1d
np.in1d

m = np.array([1,2,3,4,5])
n = np.array([3,4,5,6,7])

np.union1d(m,n)
     
array([1, 2, 3, 4, 5, 6, 7])

np.intersect1d(m,n)
     
array([3, 4, 5])

np.setdiff1d(n,m)
     
array([6, 7])

np.setxor1d(m,n)
     
array([1, 2, 6, 7])

m[np.in1d(m,1)]
     
array([1])
np.clip
numpy.clip() function is used to Clip (limit) the values in an array.

https://numpy.org/doc/stable/reference/generated/numpy.clip.html


# code
a
     
array([110, 530,  28,  50,  38,  37,  94,  92,   5,  30,  68,   9,  78,
         2,  21])

np.clip(a,a_min=25,a_max=75)
     
array([75, 75, 28, 50, 38, 37, 75, 75, 25, 30, 68, 25, 75, 25, 25])

# 17. np.swapaxes
     

# 18. np.uniform
     

# 19. np.count_nonzero
     

# 21. np.tile
# https://www.kaggle.com/code/abhayparashar31/best-numpy-functions-for-data-science-50?scriptVersionId=98816580
     

# 22. np.repeat
# https://towardsdatascience.com/10-numpy-functions-you-should-know-1dc4863764c5
     


# 25. np.allclose and equals
     


     


     


     


     


     


     
###################################################################################



What is Pandas
Pandas is a fast, powerful, flexible and easy to use open source data analysis and manipulation tool, built on top of the Python programming language.

https://pandas.pydata.org/about/index.html

Pandas Series
A Pandas Series is like a column in a table. It is a 1-D array holding data of any type.

Importing Pandas

import numpy as np
import pandas as pd
     
Series from lists

# string
country = ['India','Pakistan','USA','Nepal','Srilanka']

pd.Series(country)
     
0       India
1    Pakistan
2         USA
3       Nepal
4    Srilanka
dtype: object

# integers
runs = [13,24,56,78,100]

runs_ser = pd.Series(runs)
     

# custom index
marks = [67,57,89,100]
subjects = ['maths','english','science','hindi']

pd.Series(marks,index=subjects)
     
maths       67
english     57
science     89
hindi      100
dtype: int64

# setting a name
marks = pd.Series(marks,index=subjects,name='Nitish ke marks')
marks
     
maths       67
english     57
science     89
hindi      100
Name: Nitish ke marks, dtype: int64
Series from dict

marks = {
    'maths':67,
    'english':57,
    'science':89,
    'hindi':100
}

marks_series = pd.Series(marks,name='nitish ke marks')
marks_series
     
maths       67
english     57
science     89
hindi      100
Name: nitish ke marks, dtype: int64
Series Attributes

# size
marks_series.size
     
4

# dtype
marks_series.dtype
     
dtype('int64')

# name
marks_series.name
     
'nitish ke marks'

# is_unique
marks_series.is_unique

pd.Series([1,1,2,3,4,5]).is_unique
     
False

# index
marks_series.index
     
Index(['maths', 'english', 'science', 'hindi'], dtype='object')

runs_ser.index
     
RangeIndex(start=0, stop=5, step=1)

# values
marks_series.values
     
array([ 67,  57,  89, 100])
Series using read_csv

# with one col
subs = pd.read_csv('/content/subs.csv',squeeze=True)
subs
     
0       48
1       57
2       40
3       43
4       44
      ... 
360    231
361    226
362    155
363    144
364    172
Name: Subscribers gained, Length: 365, dtype: int64

# with 2 cols
vk = pd.read_csv('/content/kohli_ipl.csv',index_col='match_no',squeeze=True)
vk
     
match_no
1       1
2      23
3      13
4      12
5       1
       ..
211     0
212    20
213    73
214    25
215     7
Name: runs, Length: 215, dtype: int64

movies = pd.read_csv('/content/bollywood.csv',index_col='movie',squeeze=True)
movies
     
movie
Uri: The Surgical Strike                   Vicky Kaushal
Battalion 609                                Vicky Ahuja
The Accidental Prime Minister (film)         Anupam Kher
Why Cheat India                            Emraan Hashmi
Evening Shadows                         Mona Ambegaonkar
                                              ...       
Hum Tumhare Hain Sanam                    Shah Rukh Khan
Aankhen (2002 film)                     Amitabh Bachchan
Saathiya (film)                             Vivek Oberoi
Company (film)                                Ajay Devgn
Awara Paagal Deewana                        Akshay Kumar
Name: lead, Length: 1500, dtype: object
Series methods

# head and tail
subs.head()
     
0    48
1    57
2    40
3    43
4    44
Name: Subscribers gained, dtype: int64

vk.head(3)
     
match_no
1     1
2    23
3    13
Name: runs, dtype: int64

vk.tail(10)
     
match_no
206     0
207     0
208     9
209    58
210    30
211     0
212    20
213    73
214    25
215     7
Name: runs, dtype: int64

# sample
movies.sample(5)
     
movie
Arjun: The Warrior Prince    Yudhveer Bakoliya
Viceroy's House (film)         Hugh Bonneville
Joggers' Park (film)           Victor Banerjee
Tere Mere Phere                   Vinay Pathak
Mission Mangal                    Akshay Kumar
Name: lead, dtype: object

# value_counts -> movies
movies.value_counts()
     
Akshay Kumar        48
Amitabh Bachchan    45
Ajay Devgn          38
Salman Khan         31
Sanjay Dutt         26
                    ..
Diganth              1
Parveen Kaur         1
Seema Azmi           1
Akanksha Puri        1
Edwin Fernandes      1
Name: lead, Length: 566, dtype: int64

# sort_values -> inplace
vk.sort_values(ascending=False).head(1).values[0]
     
113

vk.sort_values(ascending=False)
     
match_no
128    113
126    109
123    108
164    100
120    100
      ... 
93       0
211      0
130      0
8        0
135      0
Name: runs, Length: 215, dtype: int64

# sort_index -> inplace -> movies
movies.sort_index(ascending=False,inplace=True)
     

movies
     
movie
Zor Lagaa Ke...Haiya!            Meghan Jadhav
Zokkomon                       Darsheel Safary
Zindagi Tere Naam           Mithun Chakraborty
Zindagi Na Milegi Dobara        Hrithik Roshan
Zindagi 50-50                      Veena Malik
                                   ...        
2 States (2014 film)              Arjun Kapoor
1971 (2007 film)                Manoj Bajpayee
1920: The Evil Returns             Vicky Ahuja
1920: London                     Sharman Joshi
1920 (film)                   Rajniesh Duggall
Name: lead, Length: 1500, dtype: object

vk.sort_values(inplace=True)
     

vk
     
match_no
87       0
211      0
207      0
206      0
91       0
      ... 
164    100
120    100
123    108
126    109
128    113
Name: runs, Length: 215, dtype: int64
Series Maths Methods

# count
vk.count()
     
215

# sum -> product
subs.sum()
     
49510

# mean -> median -> mode -> std -> var
subs.mean()
print(vk.median())
print(movies.mode())
print(subs.std())
print(vk.var())
     
24.0
0    Akshay Kumar
dtype: object
62.6750230372527
688.0024777222343

# min/max
subs.max()
     
396

# describe
subs.describe()
     
count    365.000000
mean     135.643836
std       62.675023
min       33.000000
25%       88.000000
50%      123.000000
75%      177.000000
max      396.000000
Name: Subscribers gained, dtype: float64
Series Indexing

# integer indexing
x = pd.Series([12,13,14,35,46,57,58,79,9])
x
     
0    12
1    13
2    14
3    35
4    46
5    57
6    58
7    79
8     9
dtype: int64

# negative indexing
x[-1]
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/range.py in get_loc(self, key, method, tolerance)
    384                 try:
--> 385                     return self._range.index(new_key)
    386                 except ValueError as err:

ValueError: -1 is not in range

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
<ipython-input-103-60055cb99cf0> in <module>
      1 # negative indexing
----> 2 x[-1]

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in __getitem__(self, key)
    940 
    941         elif key_is_scalar:
--> 942             return self._get_value(key)
    943 
    944         if is_hashable(key):

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in _get_value(self, label, takeable)
   1049 
   1050         # Similar to Index.get_value, but we do not fall back to positional
-> 1051         loc = self.index.get_loc(label)
   1052         return self.index._get_values_for_loc(self, loc, label)
   1053 

/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/range.py in get_loc(self, key, method, tolerance)
    385                     return self._range.index(new_key)
    386                 except ValueError as err:
--> 387                     raise KeyError(key) from err
    388             raise KeyError(key)
    389         return super().get_loc(key, method=method, tolerance=tolerance)

KeyError: -1

movies
     
movie
Zor Lagaa Ke...Haiya!            Meghan Jadhav
Zokkomon                       Darsheel Safary
Zindagi Tere Naam           Mithun Chakraborty
Zindagi Na Milegi Dobara        Hrithik Roshan
Zindagi 50-50                      Veena Malik
                                   ...        
2 States (2014 film)              Arjun Kapoor
1971 (2007 film)                Manoj Bajpayee
1920: The Evil Returns             Vicky Ahuja
1920: London                     Sharman Joshi
1920 (film)                   Rajniesh Duggall
Name: lead, Length: 1500, dtype: object

vk[-1]
     
---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3360             try:
-> 3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.Int64HashTable.get_item()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.Int64HashTable.get_item()

KeyError: -1

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
<ipython-input-107-8ecb27b68523> in <module>
----> 1 vk[-1]

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in __getitem__(self, key)
    940 
    941         elif key_is_scalar:
--> 942             return self._get_value(key)
    943 
    944         if is_hashable(key):

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in _get_value(self, label, takeable)
   1049 
   1050         # Similar to Index.get_value, but we do not fall back to positional
-> 1051         loc = self.index.get_loc(label)
   1052         return self.index._get_values_for_loc(self, loc, label)
   1053 

/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:
-> 3363                 raise KeyError(key) from err
   3364 
   3365         if is_scalar(key) and isna(key) and not self.hasnans:

KeyError: -1

marks_series[-1]
     
100

# slicing
vk[5:16]
     
match_no
6      9
7     34
8      0
9     21
10     3
11    10
12    38
13     3
14    11
15    50
16     2
Name: runs, dtype: int64

# negative slicing
vk[-5:]
     
match_no
211     0
212    20
213    73
214    25
215     7
Name: runs, dtype: int64

movies[::2]
     
movie
Zor Lagaa Ke...Haiya!         Meghan Jadhav
Zindagi Tere Naam        Mithun Chakraborty
Zindagi 50-50                   Veena Malik
Zinda (film)                    Sanjay Dutt
Zid (2014 film)              Mannara Chopra
                                ...        
3 Storeys                       Aisha Ahmed
3 Deewarein                Naseeruddin Shah
22 Yards                        Barun Sobti
1971 (2007 film)             Manoj Bajpayee
1920: London                  Sharman Joshi
Name: lead, Length: 750, dtype: object

# fancy indexing
vk[[1,3,4,5]]
     
match_no
1     1
3    13
4    12
5     1
Name: runs, dtype: int64

# indexing with labels -> fancy indexing
movies['2 States (2014 film)']
     
'Arjun Kapoor'
Editing Series

# using indexing
marks_series[1] = 100
marks_series
     
maths       67
english    100
science     89
hindi      100
Name: nitish ke marks, dtype: int64

# what if an index does not exist
marks_series['evs'] = 100
     

marks_series
     
maths       67
english    100
science     89
hindi      100
sst         90
evs        100
Name: nitish ke marks, dtype: int64

# slicing
runs_ser[2:4] = [100,100]
runs_ser
     
0     13
1     24
2    100
3    100
4    100
dtype: int64

# fancy indexing
runs_ser[[0,3,4]] = [0,0,0]
runs_ser
     
0      0
1     24
2    100
3      0
4      0
dtype: int64

# using index label
movies['2 States (2014 film)'] = 'Alia Bhatt'
movies
     
movie
Zor Lagaa Ke...Haiya!            Meghan Jadhav
Zokkomon                       Darsheel Safary
Zindagi Tere Naam           Mithun Chakraborty
Zindagi Na Milegi Dobara        Hrithik Roshan
Zindagi 50-50                      Veena Malik
                                   ...        
2 States (2014 film)                Alia Bhatt
1971 (2007 film)                Manoj Bajpayee
1920: The Evil Returns             Vicky Ahuja
1920: London                     Sharman Joshi
1920 (film)                   Rajniesh Duggall
Name: lead, Length: 1500, dtype: object
Copy and Views


     
Series with Python Functionalities

# len/type/dir/sorted/max/min
print(len(subs))
print(type(subs))
print(dir(subs))
print(sorted(subs))
print(min(subs))
print(max(subs))
     
365
<class 'pandas.core.series.Series'>
['T', '_AXIS_LEN', '_AXIS_ORDERS', '_AXIS_REVERSED', '_AXIS_TO_AXIS_NUMBER', '_HANDLED_TYPES', '__abs__', '__add__', '__and__', '__annotations__', '__array__', '__array_priority__', '__array_ufunc__', '__array_wrap__', '__bool__', '__class__', '__contains__', '__copy__', '__deepcopy__', '__delattr__', '__delitem__', '__dict__', '__dir__', '__divmod__', '__doc__', '__eq__', '__finalize__', '__float__', '__floordiv__', '__format__', '__ge__', '__getattr__', '__getattribute__', '__getitem__', '__getstate__', '__gt__', '__hash__', '__iadd__', '__iand__', '__ifloordiv__', '__imod__', '__imul__', '__init__', '__init_subclass__', '__int__', '__invert__', '__ior__', '__ipow__', '__isub__', '__iter__', '__itruediv__', '__ixor__', '__le__', '__len__', '__long__', '__lt__', '__matmul__', '__mod__', '__module__', '__mul__', '__ne__', '__neg__', '__new__', '__nonzero__', '__or__', '__pos__', '__pow__', '__radd__', '__rand__', '__rdivmod__', '__reduce__', '__reduce_ex__', '__repr__', '__rfloordiv__', '__rmatmul__', '__rmod__', '__rmul__', '__ror__', '__round__', '__rpow__', '__rsub__', '__rtruediv__', '__rxor__', '__setattr__', '__setitem__', '__setstate__', '__sizeof__', '__str__', '__sub__', '__subclasshook__', '__truediv__', '__weakref__', '__xor__', '_accessors', '_accum_func', '_add_numeric_operations', '_agg_by_level', '_agg_examples_doc', '_agg_see_also_doc', '_align_frame', '_align_series', '_arith_method', '_as_manager', '_attrs', '_binop', '_can_hold_na', '_check_inplace_and_allows_duplicate_labels', '_check_inplace_setting', '_check_is_chained_assignment_possible', '_check_label_or_level_ambiguity', '_check_setitem_copy', '_clear_item_cache', '_clip_with_one_bound', '_clip_with_scalar', '_cmp_method', '_consolidate', '_consolidate_inplace', '_construct_axes_dict', '_construct_axes_from_arguments', '_construct_result', '_constructor', '_constructor_expanddim', '_convert', '_convert_dtypes', '_data', '_dir_additions', '_dir_deletions', '_drop_axis', '_drop_labels_or_levels', '_duplicated', '_find_valid_index', '_flags', '_from_mgr', '_get_axis', '_get_axis_name', '_get_axis_number', '_get_axis_resolvers', '_get_block_manager_axis', '_get_bool_data', '_get_cacher', '_get_cleaned_column_resolvers', '_get_index_resolvers', '_get_label_or_level_values', '_get_numeric_data', '_get_value', '_get_values', '_get_values_tuple', '_get_with', '_gotitem', '_hidden_attrs', '_index', '_indexed_same', '_info_axis', '_info_axis_name', '_info_axis_number', '_init_dict', '_init_mgr', '_inplace_method', '_internal_names', '_internal_names_set', '_is_cached', '_is_copy', '_is_label_or_level_reference', '_is_label_reference', '_is_level_reference', '_is_mixed_type', '_is_view', '_item_cache', '_ixs', '_logical_func', '_logical_method', '_map_values', '_maybe_update_cacher', '_memory_usage', '_metadata', '_mgr', '_min_count_stat_function', '_name', '_needs_reindex_multi', '_protect_consolidate', '_reduce', '_reindex_axes', '_reindex_indexer', '_reindex_multi', '_reindex_with_indexers', '_replace_single', '_repr_data_resource_', '_repr_latex_', '_reset_cache', '_reset_cacher', '_set_as_cached', '_set_axis', '_set_axis_name', '_set_axis_nocheck', '_set_is_copy', '_set_labels', '_set_name', '_set_value', '_set_values', '_set_with', '_set_with_engine', '_slice', '_stat_axis', '_stat_axis_name', '_stat_axis_number', '_stat_function', '_stat_function_ddof', '_take_with_is_copy', '_typ', '_update_inplace', '_validate_dtype', '_values', '_where', 'abs', 'add', 'add_prefix', 'add_suffix', 'agg', 'aggregate', 'align', 'all', 'any', 'append', 'apply', 'argmax', 'argmin', 'argsort', 'array', 'asfreq', 'asof', 'astype', 'at', 'at_time', 'attrs', 'autocorr', 'axes', 'backfill', 'between', 'between_time', 'bfill', 'bool', 'clip', 'combine', 'combine_first', 'compare', 'convert_dtypes', 'copy', 'corr', 'count', 'cov', 'cummax', 'cummin', 'cumprod', 'cumsum', 'describe', 'diff', 'div', 'divide', 'divmod', 'dot', 'drop', 'drop_duplicates', 'droplevel', 'dropna', 'dtype', 'dtypes', 'duplicated', 'empty', 'eq', 'equals', 'ewm', 'expanding', 'explode', 'factorize', 'ffill', 'fillna', 'filter', 'first', 'first_valid_index', 'flags', 'floordiv', 'ge', 'get', 'groupby', 'gt', 'hasnans', 'head', 'hist', 'iat', 'idxmax', 'idxmin', 'iloc', 'index', 'infer_objects', 'interpolate', 'is_monotonic', 'is_monotonic_decreasing', 'is_monotonic_increasing', 'is_unique', 'isin', 'isna', 'isnull', 'item', 'items', 'iteritems', 'keys', 'kurt', 'kurtosis', 'last', 'last_valid_index', 'le', 'loc', 'lt', 'mad', 'map', 'mask', 'max', 'mean', 'median', 'memory_usage', 'min', 'mod', 'mode', 'mul', 'multiply', 'name', 'nbytes', 'ndim', 'ne', 'nlargest', 'notna', 'notnull', 'nsmallest', 'nunique', 'pad', 'pct_change', 'pipe', 'plot', 'pop', 'pow', 'prod', 'product', 'quantile', 'radd', 'rank', 'ravel', 'rdiv', 'rdivmod', 'reindex', 'reindex_like', 'rename', 'rename_axis', 'reorder_levels', 'repeat', 'replace', 'resample', 'reset_index', 'rfloordiv', 'rmod', 'rmul', 'rolling', 'round', 'rpow', 'rsub', 'rtruediv', 'sample', 'searchsorted', 'sem', 'set_axis', 'set_flags', 'shape', 'shift', 'size', 'skew', 'slice_shift', 'sort_index', 'sort_values', 'squeeze', 'std', 'sub', 'subtract', 'sum', 'swapaxes', 'swaplevel', 'tail', 'take', 'to_clipboard', 'to_csv', 'to_dict', 'to_excel', 'to_frame', 'to_hdf', 'to_json', 'to_latex', 'to_list', 'to_markdown', 'to_numpy', 'to_period', 'to_pickle', 'to_sql', 'to_string', 'to_timestamp', 'to_xarray', 'transform', 'transpose', 'truediv', 'truncate', 'tz_convert', 'tz_localize', 'unique', 'unstack', 'update', 'value_counts', 'values', 'var', 'view', 'where', 'xs']
[33, 33, 35, 37, 39, 40, 40, 40, 40, 42, 42, 43, 44, 44, 44, 45, 46, 46, 48, 49, 49, 49, 49, 50, 50, 50, 51, 54, 56, 56, 56, 56, 57, 61, 62, 64, 65, 65, 66, 66, 66, 66, 67, 68, 70, 70, 70, 71, 71, 72, 72, 72, 72, 72, 73, 74, 74, 75, 76, 76, 76, 76, 77, 77, 78, 78, 78, 79, 79, 80, 80, 80, 81, 81, 82, 82, 83, 83, 83, 84, 84, 84, 85, 86, 86, 86, 87, 87, 87, 87, 88, 88, 88, 88, 88, 89, 89, 89, 90, 90, 90, 90, 91, 92, 92, 92, 93, 93, 93, 93, 95, 95, 96, 96, 96, 96, 97, 97, 98, 98, 99, 99, 100, 100, 100, 101, 101, 101, 102, 102, 103, 103, 104, 104, 104, 105, 105, 105, 105, 105, 105, 105, 105, 105, 108, 108, 108, 108, 108, 108, 109, 109, 110, 110, 110, 111, 111, 112, 113, 113, 113, 114, 114, 114, 114, 115, 115, 115, 115, 117, 117, 117, 118, 118, 119, 119, 119, 119, 120, 122, 123, 123, 123, 123, 123, 124, 125, 126, 127, 128, 128, 129, 130, 131, 131, 132, 132, 134, 134, 134, 135, 135, 136, 136, 136, 137, 138, 138, 138, 139, 140, 144, 145, 146, 146, 146, 146, 147, 149, 150, 150, 150, 150, 151, 152, 152, 152, 153, 153, 153, 154, 154, 154, 155, 155, 156, 156, 156, 156, 157, 157, 157, 157, 158, 158, 159, 159, 160, 160, 160, 160, 162, 164, 166, 167, 167, 168, 170, 170, 170, 170, 171, 172, 172, 173, 173, 173, 174, 174, 175, 175, 176, 176, 177, 178, 179, 179, 180, 180, 180, 182, 183, 183, 183, 184, 184, 184, 185, 185, 185, 185, 186, 186, 186, 188, 189, 190, 190, 192, 192, 192, 196, 196, 196, 197, 197, 202, 202, 202, 203, 204, 206, 207, 209, 210, 210, 211, 212, 213, 214, 216, 219, 220, 221, 221, 222, 222, 224, 225, 225, 226, 227, 228, 229, 230, 231, 233, 236, 236, 237, 241, 243, 244, 245, 247, 249, 254, 254, 258, 259, 259, 261, 261, 265, 267, 268, 269, 276, 276, 290, 295, 301, 306, 312, 396]
33
396

# type conversion
list(marks_series)
     
[67, 100, 89, 100, 90, 100]

dict(marks_series)
     
{'maths': 67,
 'english': 100,
 'science': 89,
 'hindi': 100,
 'sst': 90,
 'evs': 100}

# membership operator

'2 States (2014 film)' in movies
     
True

'Alia Bhatt' in movies.values
     
True

movies
     
movie
Zor Lagaa Ke...Haiya!            Meghan Jadhav
Zokkomon                       Darsheel Safary
Zindagi Tere Naam           Mithun Chakraborty
Zindagi Na Milegi Dobara        Hrithik Roshan
Zindagi 50-50                      Veena Malik
                                   ...        
2 States (2014 film)                Alia Bhatt
1971 (2007 film)                Manoj Bajpayee
1920: The Evil Returns             Vicky Ahuja
1920: London                     Sharman Joshi
1920 (film)                   Rajniesh Duggall
Name: lead, Length: 1500, dtype: object

# looping
for i in movies.index:
  print(i)
     
Zor Lagaa Ke...Haiya!
Zokkomon
Zindagi Tere Naam
Zindagi Na Milegi Dobara
Zindagi 50-50
Zindaggi Rocks
Zinda (film)
Zila Ghaziabad
Zid (2014 film)
Zero (2018 film)
Zeher
Zed Plus
Zameer: The Fire Within
Zameen (2003 film)
Zamaanat
Yuvvraaj
Yuva
Yun Hota Toh Kya Hota
Youngistaan
Yeh Saali Aashiqui
Yeh Mera India
Yeh Lamhe Judaai Ke
Yeh Khula Aasmaan
Yeh Jawaani Hai Deewani
Yeh Hai India
Yeh Hai Bakrapur
Yeh Dooriyan
Yeh Dil
Yatra (2007 film)
Yamla Pagla Deewana: Phir Se
Yamla Pagla Deewana
Yakeen (2005 film)
Yadvi – The Dignified Princess
Yaaram (2019 film)
Ya Rab
Xcuse Me
Woodstock Villa
Woh Lamhe...
Why Cheat India
What's Your Raashee?
What the Fish
Well Done Abba
Welcome to Sajjanpur
Welcome Back (film)
Welcome 2 Karachi
Welcome (2007 film)
Wedding Pullav
Wedding Anniversary
Waris Shah: Ishq Daa Waaris
War Chhod Na Yaar
Waqt: The Race Against Time
Wanted (2009 film)
Wake Up Sid
Wake Up India
Wajah Tum Ho
Waiting (2015 film)
Waisa Bhi Hota Hai Part II
Wah Taj
Wafa: A Deadly Love Story
Waarrior Savitri
W (2014 film)
Vodka Diaries
Vivah
Vishwaroopam
Viruddh... Family Comes First
Vidyaarthi
Victory (2009 film)
Vicky Donor
Viceroy's House (film)
Via Darjeeling
Veerey Ki Wedding
Veerappan (2016 film)
Veer-Zaara
Veer (2010 film)
Valentine's Night
Vaastu Shastra (film)
Vaah! Life Ho Toh Aisi!
Vaada Raha
Vaada (film)
Uvaa
Utthaan
Utt Pataang
Uri: The Surgical Strike
United Six
Union Leader (film)
Ungli
Umrika
Umrao Jaan (2006 film)
Umar (film)
Ujda Chaman
Ugly (film)
Udta Punjab
Udaan (2010 film)
U R My Jaan
U Me Aur Hum
Turning 30
Tumsa Nahin Dekha: A Love Story
Tumhari Sulu
Tumbbad
Tum Milo Toh Sahi
Tum Mile
Tum Bin II
Tulsi (film)
Tujhe Meri Kasam
Tubelight (2017 Hindi film)
Trump Card (film)
Trapped (2016 Hindi film)
Traffic Signal (film)
Traffic (2016 film)
Total Siyapaa
Total Dhamaal
Toonpur Ka Super Hero
Tom Dick and Harry (2006 film)
Toilet: Ek Prem Katha
Toh Baat Pakki!
Titoo MBA
Titli (2014 film)
Tiger Zinda Hai
Thugs of Hindostan
Thodi Life Thoda Magic
Thoda Tum Badlo Thoda Hum
Thoda Pyaar Thoda Magic
Thoda Lutf Thoda Ishq
The Zoya Factor (film)
The Xposé
The Train (2007 film)
The Tashkent Files
The Stoneman Murders
The Sky Is Pink
The Silent Heroes
The Shaukeens
The Pink Mirror
The Namesake (film)
The Lunchbox
The Last Lear
The Killer (2006 film)
The Journey of Karma
The Japanese Wife
The Hero: Love Story of a Spy
The Ghazi Attack
The Final Exit
The Film Emotional Atyachar
The Film
The Dirty Picture
The Bypass
The Blueberry Hunt
The Blue Umbrella (2005 film)
The Accidental Prime Minister (film)
Thanks Maa
Thank You (2011 film)
Thackeray (film)
Tezz
Tevar
Teri Meri Kahaani (film)
Tere Naam
Tere Naal Love Ho Gaya
Tere Mere Phere
Tere Bin Laden: Dead or Alive
Tere Bin Laden
Tera Kya Hoga Johnny
Tell Me O Kkhuda
Tehzeeb (2003 film)
Teesri Aankh: The Hidden Camera
Tees Maar Khan (2010 film)
Teen Thay Bhai
Teen Patti (film)
Te3n
Taxi No. 9211
Tathastu
Tashan (film)
Tanu Weds Manu: Returns
Tanu Weds Manu: Returns
Tanu Weds Manu
Tango Charlie
Tamanchey
Talvar (film)
Talaash: The Hunt Begins...
Talaash: The Answer Lies Within
Take It Easy (2015 film)
Taj Mahal: An Eternal Love Story
Tahaan
Table No. 21
Taarzan: The Wonder Car
Taare Zameen Par
Ta Ra Rum Pum
Sweetiee Weds NRI
Swami (2007 film)
Swades
Super Nani
Super Model (film)
Super 30 (film)
Suno Sasurjee
Sunglass (film)
Sunday (2008 film)
Summer 2007
Sultan (2016 film)
Sulemani Keeda
Sukhmani: Hope for Life
Sui Dhaaga
Stumped (film)
Student of the Year 2
Student of the Year
Strings of Passion
Striker (2010 film)
Stree (2018 film)
Strangers (2007 Hindi film)
Staying Alive (2012 film)
Station (2014 film)
Stanley Ka Dabba
Ssukh
Sssshhh...
Speed (2007 film)
Special 26
Spark (2014 film)
Souten: The Other Woman
Sorry Daddy
Sorry Bhai!
Sooper Se Ooper
Sonu Ke Titu Ki Sweety
Sons of Ram
Soni (film)
Sonchiriya
Sonali Cable
Son of Sardaar
Socha Na Tha
Soch Lo
Sixteen (2013 Indian film)
Sirf (film)
Singham Returns
Singham
Singh Saab the Great
Singh Is Kinng
Singh Is Bliing
Simran (film)
Simmba
Silsiilay
Sikandar (2009 film)
Siddharth (2013 film)
Shukriya: Till Death Do Us Apart
Shuddh Desi Romance
Shubh Mangal Saavdhan
Showbiz (film)
Shortkut
Shortcut Safari
Shortcut Romeo
Shorgul
Shor in the City
Shootout at Lokhandwala
Sholay
Shivaay
Shiva (2006 film)
Shirin Farhad Ki Toh Nikal Padi
Ship of Theseus (film)
Shikhar (film)
Sheesha (2005 film)
Sheen (film)
Shart: The Challenge
Sharafat Gayi Tel Lene
Shanghai (2012 film)
Shamitabh
Shakalaka Boom Boom
Shaitan (film)
Shahid (film)
Shagird (2011 film)
Shabri
Shabnam Mausi
Shabd (film)
Shab (film)
Shaapit
Shaandaar
Shaadi Se Pehle
Shaadi No. 1
Shaadi Mein Zaroor Aana
Shaadi Ke Side Effects
Shaadi Karke Phas Gaya Yaar
Shaadi Ka Laddoo
Setters (film)
Sehar
Section 375
Secret Superstar
Second Hand Husband
Say Salaam India
Satyameva Jayate (2018 film)
Satyagraha (film)
Satya 2
Satta (film)
Satrangee Parachute
Satellite Shankar
Sarkar Raj
Sarkar 3
Sarkar (2005 film)
Sarhad Paar
Sarbjit (film)
Santa Banta Pvt Ltd
Sankat City
Sanju
Sandwich (2006 film)
Sanam Re
Samrat & Co.
Samay: When Time Strikes
Sallu Ki Shaadi
Salaam-e-Ishq: A Tribute to Love
Salaam Namaste
Sahi Dhandhe Galat Bande
Saheb Biwi Aur Gangster Returns
Saheb Biwi Aur Gangster 3
Saheb Biwi Aur Gangster
Sadiyaan
Sadda Adda
Sacred Evil – A True Story
Sachin: A Billion Dreams
Sabki Bajegi Band
Saaya (2003 film)
Saawariya
Saawan... The Love Season
Saathiya (film)
Saat Uchakkey
Saas Bahu Aur Sensex
Saare Jahaan Se Mehnga
Saansein
Saankal
Saand Ki Aankh
Saaho
Rustom (film)
Rush (2012 film)
Running Shaadi
Run (2004 film)
Rules: Pyaar Ka Superhit Formula
Rukh (film)
Rudraksh (film)
Roy (film)
Rough Book
Rokkk
Rok Sako To Rok Lo
Rog
Rocky Handsome
Rockstar (2011 film)
Rocket Singh: Salesman of the Year
Rock On!!
Rock On 2
Roar: Tigers of the Sundarbans
Roadside Romeo
Road to Sangam
Riyasat (film)
Risknamaa
Risk (2007 film)
Right Yaaa Wrong
Right Here Right Now (film)
Ribbon (film)
Revolver Rani
Revati (film)
Red: The Dark Side
Red Swastik
Red Alert: The War Within
Rebellious Flower
Rascals (2011 film)
Raqeeb
Rann (film)
Rangrezz
Rangoon (2017 Hindi film)
Rang Rasiya
Rang De Basanti
Ranchi Diaries
Ranbanka
Ramprasad Ki Tehrvi
Ramji Londonwaley
Ramayana: The Epic
Raman Raghav 2.0
Ramaiya Vastavaiya
Ramaa: The Saviour
Rakhtbeej
Rakht
Rajma Chawal
Rajjo
Raja Natwarlal
Raja Bhaiya (film)
Raincoat (film)
Raid (2018 film)
Rahasya
Ragini MMS 2
Ragini MMS
Raghu Romeo
Raees (film)
Race 3
Race 2
Race (2008 film)
Rab Ne Bana Di Jodi
Raazi
Raaz: The Mystery Continues
Raaz: Reboot
Raaz (2002 film)
Raavan
Raat Gayi Baat Gayi?
Raanjhanaa
Raag Desh (film)
Raabta (film)
Ra.One
Quick Gun Murugun
Queen (2014 film)
Qissa (film)
Qayamat: City Under Threat
Qarib Qarib Singlle
Qaidi Band
Pyare Mohan
Pyaar Mein Twist
Pyaar Ke Side Effects
Pyaar Ka Punchnama 2
Pyaar Ka Punchnama
Pyaar Impossible!
Purani Jeans
Prince (2010 film)
Prem Ratan Dhan Payo
Prem Kaa Game
Prateeksha
Prassthanam
Pranaam
Prague (2013 film)
Praan Jaye Par Shaan Na Jaye
Poster Boys
Popcorn Khao! Mast Ho Jao
Policegiri
Police Force: An Inside Story
Players (2012 film)
Plan (film)
Pizza (2014 film)
Pink (2016 film)
Pinjar (film)
Piku
Pihu
Photograph (film)
Phoonk 2
Phobia (2016 film)
Phir Milenge
Phir Kabhi
Phir Hera Pheri
Phillauri (film)
Phhir
Phata Poster Nikhla Hero
Phas Gaye Re Obama
Phantom (2015 film)
Phamous
Pehchaan: The Face of Truth
Peepli Live
Paying Guests
Patiala House (film)
Pati Patni Aur Woh (2019 film)
Patel Ki Punjabi Shaadi
Pataakha
Parwana (2003 film)
Partner (2007 film)
Parmanu: The Story of Pokhran
Parineeta (2005 film)
Parched
Paranthe Wali Gali
Pankh
Panchlait
Paltan (film)
Pal Pal Dil Ke Paas
Paisa Vasool
Paheli
Page 3 (film)
Pagalpanti (2019 film)
Padmashree Laloo Prasad Yadav
Padmaavat
Paathshaala
Paap
Paanch Ghantey Mien Paanch Crore
Paan Singh Tomar (film)
Paa (film)
PM Narendra Modi
PK (film)
P Se Pyaar F Se Faraar
P Se PM Tak
Oye Lucky! Lucky Oye!
Out of Control (2003 film)
One by Two (2014 film)
One Two Three
One Day: Justice Delivered
Once Upon ay Time in Mumbai Dobaara!
Once Upon a Time in Mumbaai
Omkara (2006 film)
Omerta (film)
Om-Dar-B-Dar
Om Shanti Om
Om (2003 film)
Ok Jaanu
Oh My God (2008 film)
October (2018 film)
OMG – Oh My God!
O Teri
Nothing but Life
Notebook (2019 film)
Not a Love Story (2011 film)
Noor (film)
No Smoking (2007 film)
No Problem (2010 film)
No One Killed Jessica
No Entry
Nishabd
Nirdosh
Nil Battey Sannata
Newton (film)
New York (2009 film)
Netaji Subhas Chandra Bose: The Forgotten Hero
Nehlle Pe Dehlla
Neerja
Neal 'n' Nikki
Nayee Padosan
Nawabzaade
Nautanki Saala!
Naughty @ 40
Nasha (film)
Naqaab
Nanu Ki Jaanu
Nanhe Jaisalmer
Namastey London
Namaste England
Naksha
Naina (2005 film)
Naam Shabana
Naach (2004 film)
Na Ghar Ke Na Ghaat Ke
NH10 (film)
NH-8 Road to Nidhivan
My Wife's Murder
My Name Is Khan
My Friend Pinto
My Brother…Nikhil
My Bollywood Bride
My Birthday Song
Muskaan
Musafir (2004 film)
Murder 3
Murder 2
Murder (2004 film)
Murari the Mad Gentleman
Munna Michael
Munna Bhai M.B.B.S.
Mummy Punjabi
Mumbhai Connection
Mumbai Se Aaya Mera Dost
Mumbai Salsa
Mumbai Meri Jaan
Mumbai Matinee
Mumbai Mast Kallander
Mumbai Delhi Mumbai
Mumbai Can Dance Saala
Mumbai 125 KM
Mulk (film)
Mukkabaaz
Mukhbiir
Mujhse Shaadi Karogi
Mujhse Fraaandship Karoge
Mughal-e-Azam
Mubarakan
Mr. X (2015 film)
Mr. Singh Mrs. Mehta
Mr. Bhatti on Chutti
Mr Prime Minister
Motu Patlu: King of Kings
Motichoor Chaknachoor
Morning Raga
Monsoon Shootout
Monica (film)
Money Hai Toh Honey Hai
Mom (film)
Mohenjo Daro (film)
Mohalla Assi
Moh Maya Money
Mittal v/s Mittal
Mitron
Mission Mangal
Mission Istaanbul
Missing (2018 film)
Miss Tanakpur Haazir Ho
Mirzya (film)
Mirch
Miley Naa Miley Hum
Milenge Milenge
Mickey Virus
Meri Pyaari Bindu
Meri Biwi Ka Jawaab Nahin
Mere Pyare Prime Minister
Mere Jeevan Saathi (2006 film)
Mere Genie Uncle
Mere Dost Picture Abhi Baki Hai
Mere Dad Ki Maruti
Mere Brother Ki Dulhan
Mere Baap Pehle Aap
Mercury (film)
Memories in March
Meinu Ek Ladki Chaahiye
Meeruthiya Gangsters
Meerabai Not Out
Meenaxi: A Tale of Three Cities
Maximum (film)
Mausam (2011 film)
Matrubhoomi
Matru Ki Bijlee Ka Mandola
Mastram
Mastizaade
Masti (2004 film)
Masaan
Mary Kom (film)
Married 2 America
Market (2003 film)
Marjaavaan
Marigold (2007 film)
Margarita with a Straw
Mardaani 2
Mardaani
Mard Ko Dard Nahi Hota
Maqbool
Mantra (2016 film)
Manto (2018 film)
Manorama Six Feet Under
Manmarziyaan
Manjunath (film)
Manjhi – The Mountain Man
Manikarnika: The Queen of Jhansi
Mangal Pandey: The Rising
Malik Ek
Malamaal Weekly
Maine Pyaar Kyun Kiya?
Maine Gandhi Ko Nahin Mara
Main Tera Hero
Main Prem Ki Diwani Hoon
Main Meri Patni Aur Woh
Main Madhuri Dixit Banna Chahti Hoon
Main Krishna Hoon
Main Hoon Part-Time Killer
Main Hoon Na
Main Aurr Mrs Khanna
Main Aur Mr. Riight
Main Aisa Hi Hoon
Mai (2013 film)
Magic Magic 3D
Madras Cafe
Madhoshi
Made in China (2019 film)
Madaari
Mad About Dance
Machine (2017 film)
Machhli Jal Ki Rani Hai
Maazii
Maatr
Maan Gaye Mughal-e-Azam
MSG: The Warrior Lion Heart
MSG: The Messenger
MSG-2 The Messenger
MP3: Mera Pehla Pehla Pyaar
M.S. Dhoni: The Untold Story
M Cream
Luv U Soniyo
Luv U Alia
Lucky: No Time for Love
Lucky Kabootar
Lucknow Central
Luckhnowi Ishq
Luck by Chance
Luck (2009 film)
Loveshhuda
Love per Square Foot
Love in Bombay
Love U...Mr. Kalakaar!
Love Story 2050
Love Sonia
Love Shagun
Love Sex Aur Dhokha
Love Ke Chakkar Mein
Love Games (film)
Love Breakups Zindagi
Love Aaj Kal
Lootera
London Dreams
Loins of Punjab Presents
Login (film)
Little Zizou
Listen... Amaya
Lipstick Under My Burkha
Life in a... Metro
Life Partner
Life Mein Kabhie Kabhiee
Life Ki Toh Lag Gayi
Life Is Beautiful (2014 film)
Life Express (2010 film)
Lekar Hum Deewana Dil
Lamhaa
Lakshya (film)
Lakshmi (2014 film)
Lakeer – Forbidden Lines
Laila Majnu (2018 film)
Lahore (film)
Lage Raho Munna Bhai
Lafangey Parindey
Ladies vs Ricky Bahl
Laal Rang
Laaga Chunari Mein Daag
LOC Kargil
Kyun! Ho Gaya Na...
Kyon Ki
Kyaa Super Kool Hain Hum
Kyaa Kool Hain Hum 3
Kyaa Kool Hai Hum
Kya Love Story Hai
Kya Dilli Kya Lahore
Kushti (film)
Kurbaan (2009 film)
Kuku Mathur Ki Jhand Ho Gayi
Kudiyon Ka Hai Zamana
Kuchh Meetha Ho Jaye
Kuchh Bheege Alfaaz
Kuch Naa Kaho
Kuch Kuch Locha Hai
Kucch To Hai
Kucch Luv Jaisaa
Krrish
Krishna Cottage
Krishna Aur Kans
Krazzy 4
Koyelaanchal
Koi... Mil Gaya
Koi Mere Dil Mein Hai
Koi Aap Sa
Knock Out (2010 film)
Kites (film)
Kisse Pyaar Karoon
Kisna: The Warrior Poet
Kismat Love Paisa Dilli
Kismat Konnection
Kismat (2004 film)
Kisaan
Kis Kisko Pyaar Karoon
Kis Kis Ki Kismat
Kill Dil
Kick (2014 film)
Ki & Ka
Khwahish
Khwaabb
Khushi (2003 Hindi film)
Khuda Kasam
Khoya Khoya Chand
Khosla Ka Ghosla
Khoobsurat (2014 film)
Khichdi: The Movie
Khel – No Ordinary Game
Khel Toh Ab Shuru Hoga
Khatta Meetha (2010 film)
Khap (film)
Khamoshiyan
Khamoshi (2019 film)
Khamoshh... Khauff Ki Raat
Khamosh Pani
Khakee
Khajoor Pe Atke
Kesari (film)
Keep Safe Distance (film)
Kaun Kitne Paani Mein
Kaun Hai Jo Sapno Mein Aaya
Katti Batti
Kash Aap Hamare Hote
Kasak (2005 film)
Karzzzz
Karwaan
Karthik Calling Karthik
Karma Aur Holi
Karle Pyaar Karle
Karar: The Deal
Karam (film)
Kapoor & Sons
Kaminey
Kalyug (2005 film)
Kalank
Kal Ho Naa Ho
Kaise Kahoon Ke... Pyaar Hai
Kai Po Che!
Kahin Hai Mera Pyar
Kahaani
Kagaar: Life on the Edge
Kadvi Hawa
Kabul Express
Kabir Singh
Kabhi Alvida Naa Kehna
Kaashi in Search of Ganga
Kaante
Kaanchi: The Unbreakable
Kaalo
Kaalakaandi
Kaal (2005 film)
Kaagaz Ke Fools
Kaabil
Just Married (2007 film)
Jurm (2005 film)
Junooniyat
Junglee (2019 film)
Julie 2
Julie (2004 film)
Jugni (2016 film)
Judwaa 2
Judgementall Hai Kya
Jolly LLB
Joker (2012 film)
Johnny Gaddaar
John Day (film)
Joggers' Park (film)
Jodi Breakers
Jodhaa Akbar
Jo Hum Chahein
Jo Bole So Nihaal (film)
Jism (2003 film)
Jimmy (2008 film)
Jigyaasa
Jigariyaa
Jia Aur Jia
Jhootha Kahin Ka
Jhootha Hi Sahi
Jhoom Barabar Jhoom
Jhankaar Beats
Jhalki
Jeena Isi Ka Naam Hai (film)
Jeena Hai Toh Thok Daal
Jazbaa
Jayantabhai Ki Luv Story
Jawani Diwani: A Youthful Joyride
Jattu Engineer
Jannat (film)
Janasheen
James (2005 film)
Jalpari: The Desert Mermaid
Jalebi (film)
Jal (film)
Jajantaram Mamantaram
Jail (2009 film)
Jai Veeru
Jai Jawaan Jai Kisaan (film)
Jai Ho (film)
Jai Gangaajal
Jai Chiranjeeva
Jahan Jaaeyega Hamen Paaeyega
Jagga Jasoos
Jackpot (2013 film)
Jack and Dil
Jabariya Jodi
Jab We Met
Jab Tak Hai Jaan
Jab Harry Met Sejal
Jaane Kyun De Yaaron
Jaane Kahan Se Aayi Hai
Jaane Hoga Kya
Jaan-E-Mann
Jaal: The Trap
JD (film)
It's a Wonderful Afterlife
Issaq
Island City (2015 film)
Isi Life Mein
Ishqiya
Ishqeria
Ishqedarriyaan
Ishq Vishk
Ishq Ke Parindey
Ishq Hai Tumse
Ishq Forever
Ishq Click
Ishkq in Paris
Ishaqzaade
Irudhi Suttru
Irada (2017 film)
Iqraar by Chance
Iqbal (film)
Inteqam: The Perfect Game
Inteha (2003 film)
Insan
Insaaf: The Justice
Inkaar (2013 film)
Indu Sarkar
Indian Babu
India's Most Wanted (film)
Impatient Vivek
I See You (2006 film)
I Proud to Be an Indian
I Love NY (2015 film)
I Love Desi
I Hate Luv Storys
I Am Kalam
I Am (2010 Indian film)
Hyderabad Blues 2
Hunterrr
Hungama (2003 film)
Humshakals
Humpty Sharma Ki Dulhania
Humne Jeena Seekh Liya
Humko Tumse Pyaar Hai
Humko Deewana Kar Gaye
Hume Tumse Pyaar Kitna
Hum Tumhare Hain Sanam
Hum Tum Shabana
Hum Tum Aur Ghost
Hum Tum
Hum Hai Raahi Car Ke
Hum Chaar
Hulchul (2004 film)
Housefull 4
Housefull 2
Housefull (2010 film)
Hotel Salvation
Hostel (2011 film)
Horror Story (film)
Hope Aur Hum
Honour Killing (film)
Honeymoon Travels Pvt. Ltd.
Home Delivery
Holiday: A Soldier Is Never Off Duty
Holiday (2006 film)
Hisss
Hindi Medium
Hind Ka Napak Ko Jawab: MSG Lion Heart 2
Himmatwala (2013 film)
Highway (2014 Hindi film)
High Jack (film)
Hichki
Heyy Babyy
Hey Bro
Heropanti
Heroine (2012 film)
Heroes (2008 film)
Hero (2015 Hindi film)
Help (film)
Hello Darling
Hello (2008 film)
Helicopter Eela
Heartless (2014 film)
Hazaaron Khwaishein Aisi
Hawayein
Hawas (2004 film)
Hawaizaada
Hawaa Hawaai
Hawa (film)
Hava Aney Dey
Haunted – 3D
Hatya (2004 film)
Hattrick (film)
Hate Story 4
Hate Story 2
Hate Story
Hastey Hastey
Haseena Parkar
Hasee Toh Phasee
Hari Puttar: A Comedy of Terrors
Haraamkhor
Happy Phirr Bhag Jayegi
Happy New Year (2014 film)
Happy Husbands (2011 film)
Happy Ending (film)
Happy Bhag Jayegi
Hanuman (2005 film)
Hamid (film)
Hamari Adhuri Kahani
Halla Bol
Halkaa
Half Girlfriend (film)
Haider (film)
Hai Apna Dil Toh Awara
Haasil
Haal-e-Dil
Guzaarish (film)
Guru (2007 film)
Gunday
Gumnaam – The Mystery
Gully Boy
Gulabi Gang (film)
Gulaal (film)
Gulaab Gang
Guest iin London
Guddu Rangeela
Guddu Ki Gun
Green Card Fever
Great Grand Masti
Grand Masti
Gour Hari Dastaan
Gori Tere Pyaar Mein
Good Newwz
Good Boy Bad Boy
Gone Kesh
Golmaal: Fun Unlimited
Golmaal Returns
Golmaal Again
Gollu Aur Pappu
Goliyon Ki Raasleela Ram-Leela
Gold (2018 film)
God Tussi Great Ho
Goal (2007 Hindi film)
Go Goa Gone
Go (2007 film)
Global Baba
Girlfriend (2004 film)
Gippi
Ghost (2019 film)
Ghost (2012 film)
Ghayal: Once Again
Ghanchakkar (film)
Ghajini (2008 film)
Genius (2018 Hindi film)
Gayab
Gauri: The Unborn
Gattu
Garv: Pride & Honour
Garam Masala (2005 film)
Gangster (2006 film)
Gangs of Wasseypur – Part 2
Gangs of Wasseypur
Gangoobai
Gangaajal
Gang of Ghosts
Gandhi My Father
Game (2011 film)
Gali Guleiyan
Gabbar Is Back
Gabbar Is Back
G Kutta Se
Fun2shh... Dudes in the 10th Century
Fun – Can Be Dangerous Sometimes
Fukrey Returns
Fukrey
Fugly (film)
Fuddu
FryDay
Fruit and Nut (film)
From Sydney with Love
Fredrick (film)
Freaky Ali
Fraud Saiyaan
Fox (film)
Force 2
Force (2011 film)
Footpath (2003 film)
Fool & Final
Flavors (film)
Flat 211
Fitoor
Firangi
Firaaq
Finding Fanny
Filmistaan
Fight Club – Members Only
Fida
Fever (2016 film)
Ferrari Ki Sawaari
Fatso!
Fashion (2008 film)
Fareb (2005 film)
Fanaa (2006 film)
Fan (film)
Familywala
Family of Thakurganj
Family (2006 film)
F.A.L.T.U
Evening Shadows
Escape from Taliban
Entertainment (2014 film)
English Vinglish
Enemmy
Elaan (2005 film)
Eklavya: The Royal Guard
Ekkees Toppon Ki Salaami
Ekk Deewana Tha
Ek: The Power of One
Ek Vivaah... Aisa Bhi
Ek Villain
Ek Thi Rani Aisi Bhi
Ek Thi Daayan
Ek Tha Tiger
Ek Second... Jo Zindagi Badal De?
Ek Se Bure Do
Ek Se Badhkar Ek (2004 film)
Ek Paheli Leela
Ek Main Aur Ekk Tu
Ek Khiladi Ek Haseena (film)
Ek Kahani Julie Ki
Ek Hasina Thi (film)
Ek Haseena Thi Ek Deewana Tha
Ek Din 24 Ghante
Ek Chalis Ki Last Local
Ek Aur Ek Gyarah
Ek Alag Mausam
Ek Ajnabee
Eight: The Power of Shani
Dus Kahaniyaan
Dus
Dunno Y... Na Jaane Kyon
Dum Maaro Dum (film)
Dum Laga Ke Haisha
Dum (2003 Hindi film)
Dulha Mil Gaya
Dude Where's the Party?
Drona (2008 film)
Drishyam (2015 film)
Dreams (2006 film)
Dream Girl (2019 film)
Double Dhamaal
Double Cross (2005 film)
Dosti: Friends Forever
Dostana (2008 film)
Dor (film)
Dongari Ka Raja
Don't Stop Dreaming
Don Muthu Swami
Don 2
Don (2006 Hindi film)
Dolly Ki Doli
Dobara
Dobaara: See Your Evil
Do Lafzon Ki Kahani (film)
Do Dooni Chaar
Dishoom
Dishkiyaoon
Dirty Politics (film)
Direct Ishq
Dilwale (2015 film)
Dilliwali Zaalim Girlfriend
Dil Toh Deewana Hai
Dil Toh Baccha Hai Ji
Dil Pardesi Ho Gayaa
Dil Ne Jise Apna Kahaa
Dil Maange More
Dil Kabaddi
Dil Ka Rishta
Dil Juunglee
Dil Jo Na Keh Saka
Dil Jo Bhi Kahey...
Dil Dosti Etc
Dil Diya Hai
Dil Dhadakne Do
Dil Bole Hadippa!
Dil Bechara Pyaar Ka Maara
Dil Bechara
Dhund (2003 film)
Dhoop
Dhoondte Reh Jaaoge
Dhoom 3
Dhoom 2
Dhoom
Dhol (film)
Dhokha
Dhobi Ghat (film)
Dharti Kahe Pukar Ke (2006 film)
Dharm (film)
Dharam Sankat Mein
Dhanak
Dhamaal
Dhadak
Devi (2016 film)
Devdas (2002 Hindi film)
Devaki (2005 film)
Dev (2004 film)
Detective Byomkesh Bakshy!
Desi Kattey
Desi Boyz
Deshdrohi
Department (film)
Delhi-6
Delhi Safari
Delhi Belly (film)
Dekh Tamasha Dekh
Dehraadun Diary
Deewane Huye Paagal
Deewaar (2004 film)
Dedh Ishqiya
Dear Zindagi
Dear Maya
Dear Friend Hitler
Dear Dad (film)
Deadline: Sirf 24 Ghante
De Taali
De De Pyaar De
De Dana Dan
Days of Tafree
Dasvidaniya
Dassehra
Darwaaza Bandh Rakho
Darr @ the Mall
Darna Zaroori Hai
Darna Mana Hai
Darling (2007 Indian film)
Dangerous Ishhq
Dangal (film)
Damadamm!
Daddy Cool (2009 Hindi film)
Daddy (2017 film)
Dabangg 3
Dabangg 2
Dabangg
Daawat-e-Ishq
Daas Dev
D-Day (2013 film)
D (film)
Crook (film)
Creature 3D
Crazy Cukkad Family
Court (film)
Corporate (2006 film)
Contract (2008 film)
Company (film)
Commando: A One Man Army
Commando 3 (film)
Coffee with D
Coffee Bloom
Cocktail (2012 film)
Click (2010 film)
Classic – Dance of Love
CityLights (2014 film)
City of Gold (2010 film)
Cigarette Ki Tarah
Chura Liyaa Hai Tumne
Chup Chup Ke
Chori Chori (2003 film)
Chor Chor Super Chor
Chocolate (2005 film)
Chittagong (film)
Chintu Ji
Chingaari
Chinar Daastaan-E-Ishq
Chillar Party
Children of War (2014 film)
Children of Heaven
Chicken Curry Law
Chhota Bheem and the Throne of Bali
Chhodon Naa Yaar
Chetna: The Excitement
Chennai Express
Chehraa
Chef (2017 film)
Cheeni Kum
Chatur Singh Two Star
Chashme Baddoor (2013 film)
Chase (2010 film)
Chargesheet (film)
Charas (2004 film)
Chandni Chowk to China
Chand Sa Roshan Chehra
Chand Ke Paar Chalo (film)
Chance Pe Dance
Chamku
Chameli (film)
Chalte Chalte (2003 film)
Chalo Dilli
Challo Driver
Chalk n Duster
Chal Pichchur Banate Hain
Chal Chala Chal
Chakravyuh (2012 film)
Chak De! India
Chaarfutiya Chhokare
Chaar Din Ki Chandni
Chaalis Chauraasi
Chaahat – Ek Nasha
Cash (2007 film)
Calendar Girls (2015 film)
Calcutta Mail
Calapor (film)
C Kkompany
Bypass Road (film)
Bunty Aur Babli
Bumper Draw
Bumm Bumm Bole
Bumboo
Bullett Raja
Bullet: Ek Dhamaka
Buddha in a Traffic Jam
Buddha Mar Gaya
Bubble Gum (film)
Brothers (2015 film)
Brij Mohan Amar Rahe
Breakaway (2011 film)
Break Ke Baad
Brahman Naman
Boss (2013 Hindi film)
Border (1997 film)
Boom (film)
Bombay to Goa (2007 film)
Bombay to Bangkok
Bombay Velvet
Bombay Talkies (film)
Bombairiya
Bollywood Diaries
Bol Bachchan
Bodyguard (2011 Hindi film)
Bobby Jasoos
Bluffmaster!
Blue (2009 film)
Bloody Isshq
Blood Money (2012 film)
Blood Brothers (2007 Indian film)
Blackmail (2005 film)
Black Friday (2007 film)
Black (2005 film)
Bittoo Boss
Bioscopewala
Bin Bulaye Baraati
Billu
Big Brother (2007 film)
Bhram
Bhopal: A Prayer for Rain
Bhoothnath Returns
Bhoothnath
Bhoot Unkle
Bhoot Returns
Bhoot (film)
Bhoomi (film)
Bhool Bhulaiyaa
Bhola in Bollywood
Bheja Fry 2
Bheja Fry (film)
Bhavesh Joshi Superhero
Bhanwarey
Bhaiaji Superhit
Bhagmati (2005 film)
Bhagam Bhag
Bhaag Milkha Bhaag
Bhaag Johnny
Bezubaan Ishq
Beyond the Clouds (2017 film)
Bewakoofiyaan
Bewafaa (2005 film)
Being Cyrus
Beiimaan Love
Behen Hogi Teri
Begum Jaan
Befikre
Bbuddah... Hoga Terra Baap
Bazaar E Husn
Batti Gul Meter Chalu
Battalion 609
Batla House
Basti (film)
Bas Ek Pal
Barsaat (2005 film)
Barkhaa
Barfi!
Bareilly Ki Barfi
Bardaasht
Barah Aana
Bank Chor
Banjo (2016 film)
Bangistan
Bang Bang!
Bandook
Band Baaja Baaraat
Banaras (2006 film)
Balwinder Singh Famous Ho Gaya
Bala (2019 film)
Bajrangi Bhaijaan
Bajirao Mastani
Bajatey Raho
Baghban (2003 film)
Badrinath Ki Dulhania
Badmashiyaan
Badlapur Boys
Badlapur (film)
Badla (2019 film)
Badhaai Ho
Bachna Ae Haseeno
Bachke Rehna Re Baba
Bachche Kachche Sachche
Baby (2015 Hindi film)
Babumoshai Bandookbaaz
Babuji Ek Ticket Bambai
Babloo Happy Hai
Baazaar
Baaz: A Bird in Danger
Baat Bann Gayi
Baar Baar Dekho
Baaghi 2
Baaghi (2016 film)
Baabul (2006 film)
Baabarr
B.A. Pass
Azhar (film)
Awarapan
Awara Paagal Deewana
Aval (2017 film)
Aurangzeb (film)
Aur Pappu Paas Ho Gaya
Ata Pata Laapata
Asambhav
Aryan: Unbreakable
Article 15 (film)
Armaan (2003 film)
Arjun: The Warrior Prince
Arjun Patiala
Apne
Apna Sapna Money Money
Apna Asmaan
Apartment (film)
Apaharan
Anwar (2007 film)
Anuradha (2014 film)
Anthony Kaun Hai?
Antardwand
Anna (2016 film)
Ankur Arora Murder Case
Ankhon Dekhi
Ankahee (2006 film)
Anjaane (2005 film)
Anjaana Anjaani
Angel (2011 film)
Andhadhun
Andaaz
Andaaz
Anamika (2008 film)
Anaarkali of Aarah
Amit Sahni Ki List
Amavas
Always Kabhi Kabhi
Aloo Chaat (film)
Alone (2015 Hindi film)
Allah Ke Banday
All the Best: Fun Begins
All Is Well (2015 film)
Aligarh (film)
Albert Pinto Ko Gussa Kyun Aata Hai?
Alag
Aladin (film)
Aksar 2
Aksar
Akaash Vani
Ajji
Ajab Prem Ki Ghazab Kahani
Ajab Gazabb Love
Aiyyaa
Aiyaary
Aitraaz
Aisa Yeh Jahaan
Aisa Kyon Hota Hai?
Airlift (film)
Ahista Ahista (2006 film)
Agnipankh
Agneepath (2012 film)
Aggar (film)
Agent Vinod (2012 film)
Aetbaar
Ae Dil Hai Mushkil
Adharm (2006 film)
Action Replayy
Action Jackson (2014 film)
Acid Factory
Accident on Hill Road
Ab Tumhare Hawale Watan Saathiyo
Ab Tak Chhappan 2
Aazaan
Aasma: The Sky Is the Limit
Aashiqui.in
Aashiqui 2
Aashiq Banaya Aapne
Aashayein
Aarakshan
Aapko Pehle Bhi Kahin Dekha Hai
Aap Ki Khatir (2006 film)
Aap Kaa Surroor
Aankhen (2002 film)
Aanch
Aan: Men at Work
Aalaap (film)
Aakrosh (2010 film)
Aakhari Decision
Aaja Nachle
Aaj Ka Andha Kanoon
Aagey Se Right
Aag (2007 film)
Aabra Ka Daabra
Aa Gaya Hero
Aa Dekhen Zara
ABCD 2
A Gentleman
A Flying Jatt
A Flat (film)
A Decent Arrangement
?: A Question Mark
99.9 FM (film)
99 (2009 film)
88 Antop Hill
7½ Phere
7 Khoon Maaf
7 Hours to Go
68 Pages
5 Weddings
404 (film)
3G (film)
36 China Town
31st October (film)
3 Storeys
3 Idiots
3 Deewarein
3 A.M. (2014 film)
22 Yards
2 States (2014 film)
1971 (2007 film)
1920: The Evil Returns
1920: London
1920 (film)

# Arithmetic Operators(Broadcasting)
100 + marks_series
     
maths      167
english    200
science    189
hindi      200
sst        190
evs        200
Name: nitish ke marks, dtype: int64

# Relational Operators

vk >= 50
     
match_no
1      False
2      False
3      False
4      False
5      False
       ...  
211    False
212    False
213     True
214    False
215    False
Name: runs, Length: 215, dtype: bool
Boolean Indexing on Series

# Find no of 50's and 100's scored by kohli
vk[vk >= 50].size
     
50

# find number of ducks
vk[vk == 0].size
     
9

# Count number of day when I had more than 200 subs a day
subs[subs > 200].size
     
59

# find actors who have done more than 20 movies
num_movies = movies.value_counts()
num_movies[num_movies > 20]
     
Akshay Kumar        48
Amitabh Bachchan    45
Ajay Devgn          38
Salman Khan         31
Sanjay Dutt         26
Shah Rukh Khan      22
Emraan Hashmi       21
Name: lead, dtype: int64
Plotting Graphs on Series

subs.plot()
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f54e0531a60>


movies.value_counts().head(20).plot(kind='pie')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f54e04f6850>

Some Important Series Methods

# astype
# between
# clip
# drop_duplicates
# isnull
# dropna
# fillna
# isin
# apply
# copy
     

import numpy as np
import pandas as pd
     

subs = pd.read_csv('/content/subs.csv',squeeze=True)
subs
     
0       48
1       57
2       40
3       43
4       44
      ... 
360    231
361    226
362    155
363    144
364    172
Name: Subscribers gained, Length: 365, dtype: int64

vk = pd.read_csv('/content/kohli_ipl.csv',index_col='match_no',squeeze=True)
vk
     
match_no
1       1
2      23
3      13
4      12
5       1
       ..
211     0
212    20
213    73
214    25
215     7
Name: runs, Length: 215, dtype: int64

movies = pd.read_csv('/content/bollywood.csv',index_col='movie',squeeze=True)
movies
     
movie
Uri: The Surgical Strike                   Vicky Kaushal
Battalion 609                                Vicky Ahuja
The Accidental Prime Minister (film)         Anupam Kher
Why Cheat India                            Emraan Hashmi
Evening Shadows                         Mona Ambegaonkar
                                              ...       
Hum Tumhare Hain Sanam                    Shah Rukh Khan
Aankhen (2002 film)                     Amitabh Bachchan
Saathiya (film)                             Vivek Oberoi
Company (film)                                Ajay Devgn
Awara Paagal Deewana                        Akshay Kumar
Name: lead, Length: 1500, dtype: object

# astype
import sys
sys.getsizeof(vk)
     
3456

sys.getsizeof(vk.astype('int16'))
     
2166

# between
vk[vk.between(51,99)].size
     
43


     

# clip
subs
     
0       48
1       57
2       40
3       43
4       44
      ... 
360    231
361    226
362    155
363    144
364    172
Name: Subscribers gained, Length: 365, dtype: int64

subs.clip(100,200)
     
0      100
1      100
2      100
3      100
4      100
      ... 
360    200
361    200
362    155
363    144
364    172
Name: Subscribers gained, Length: 365, dtype: int64

# drop_duplicates
temp = pd.Series([1,1,2,2,3,3,4,4])
temp
     
0    1
1    1
2    2
3    2
4    3
5    3
6    4
7    4
dtype: int64

temp.drop_duplicates(keep='last')
     
1    1
3    2
5    3
7    4
dtype: int64

temp.duplicated().sum()
     
4

vk.duplicated().sum()
     
137

movies.drop_duplicates()
     
movie
Uri: The Surgical Strike                   Vicky Kaushal
Battalion 609                                Vicky Ahuja
The Accidental Prime Minister (film)         Anupam Kher
Why Cheat India                            Emraan Hashmi
Evening Shadows                         Mona Ambegaonkar
                                              ...       
Sssshhh...                              Tanishaa Mukerji
Rules: Pyaar Ka Superhit Formula                  Tanuja
Right Here Right Now (film)                        Ankit
Talaash: The Hunt Begins...                Rakhee Gulzar
The Pink Mirror                          Edwin Fernandes
Name: lead, Length: 566, dtype: object

temp = pd.Series([1,2,3,np.nan,5,6,np.nan,8,np.nan,10])
temp
     
0     1.0
1     2.0
2     3.0
3     NaN
4     5.0
5     6.0
6     NaN
7     8.0
8     NaN
9    10.0
dtype: float64

temp.size
     
10

temp.count()
     
7

# isnull
temp.isnull().sum()
     
3


     

# dropna
temp.dropna()
     
0     1.0
1     2.0
2     3.0
4     5.0
5     6.0
7     8.0
9    10.0
dtype: float64


     

# fillna
temp.fillna(temp.mean())
     
0     1.0
1     2.0
2     3.0
3     5.0
4     5.0
5     6.0
6     5.0
7     8.0
8     5.0
9    10.0
dtype: float64


     

# isin
vk[(vk == 49) | (vk == 99)]
     
match_no
82    99
86    49
Name: runs, dtype: int64

vk[vk.isin([49,99])]
     
match_no
82    99
86    49
Name: runs, dtype: int64


     

# apply
movies
     
movie
Uri: The Surgical Strike                   Vicky Kaushal
Battalion 609                                Vicky Ahuja
The Accidental Prime Minister (film)         Anupam Kher
Why Cheat India                            Emraan Hashmi
Evening Shadows                         Mona Ambegaonkar
                                              ...       
Hum Tumhare Hain Sanam                    Shah Rukh Khan
Aankhen (2002 film)                     Amitabh Bachchan
Saathiya (film)                             Vivek Oberoi
Company (film)                                Ajay Devgn
Awara Paagal Deewana                        Akshay Kumar
Name: lead, Length: 1500, dtype: object

movies.apply(lambda x:x.split()[0].upper())
     
movie
Uri: The Surgical Strike                  VICKY
Battalion 609                             VICKY
The Accidental Prime Minister (film)     ANUPAM
Why Cheat India                          EMRAAN
Evening Shadows                            MONA
                                         ...   
Hum Tumhare Hain Sanam                     SHAH
Aankhen (2002 film)                     AMITABH
Saathiya (film)                           VIVEK
Company (film)                             AJAY
Awara Paagal Deewana                     AKSHAY
Name: lead, Length: 1500, dtype: object

subs
     
0       48
1       57
2       40
3       43
4       44
      ... 
360    231
361    226
362    155
363    144
364    172
Name: Subscribers gained, Length: 365, dtype: int64

subs.apply(lambda x:'good day' if x > subs.mean() else 'bad day')
     
0       bad day
1       bad day
2       bad day
3       bad day
4       bad day
         ...   
360    good day
361    good day
362    good day
363    good day
364    good day
Name: Subscribers gained, Length: 365, dtype: object

subs.mean()
     
135.64383561643837

# copy
     

vk
     
match_no
1       1
2      23
3      13
4      12
5       1
       ..
211     0
212    20
213    73
214    25
215     7
Name: runs, Length: 215, dtype: int64

new = vk.head()
     

new
     
match_no
1     1
2    23
3    13
4    12
5     1
Name: runs, dtype: int64

new[1] = 1
     

new = vk.head().copy()
     

new[1] = 100
     

new
     
match_no
1    100
2     23
3     13
4     12
5      1
Name: runs, dtype: int64

vk
     
match_no
1       1
2      23
3      13
4      12
5       1
       ..
211     0
212    20
213    73
214    25
215     7
Name: runs, Length: 215, dtype: int64


     
###################################################################################




import numpy as np
import pandas as pd
     
Creating DataFrame

# using lists
student_data = [
    [100,80,10],
    [90,70,7],
    [120,100,14],
    [80,50,2]
]

pd.DataFrame(student_data,columns=['iq','marks','package'])
     
iq	marks	package
0	100	80	10
1	90	70	7
2	120	100	14
3	80	50	2

# using dicts

student_dict = {
    'name':['nitish','ankit','rupesh','rishabh','amit','ankita'],
    'iq':[100,90,120,80,0,0],
    'marks':[80,70,100,50,0,0],
    'package':[10,7,14,2,0,0]
}

students = pd.DataFrame(student_dict)
students.set_index('name',inplace=True)
students
     
iq	marks	package
name			
nitish	100	80	10
ankit	90	70	7
rupesh	120	100	14
rishabh	80	50	2
amit	0	0	0
ankita	0	0	0

# using read_csv
movies = pd.read_csv('movies.csv')
movies
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)
3	Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)
4	Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
1624	Tera Mera Saath Rahen	tt0301250	https://upload.wikimedia.org/wikipedia/en/2/2b...	https://en.wikipedia.org/wiki/Tera_Mera_Saath_...	Tera Mera Saath Rahen	Tera Mera Saath Rahen	0	2001	148	Drama	4.9	278	Raj Dixit lives with his younger brother Rahu...	A man is torn between his handicapped brother ...	NaN	Ajay Devgn|Sonali Bendre|Namrata Shirodkar|Pre...	NaN	7 November 2001 (India)
1625	Yeh Zindagi Ka Safar	tt0298607	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Zindagi_Ka_S...	Yeh Zindagi Ka Safar	Yeh Zindagi Ka Safar	0	2001	146	Drama	3.0	133	Hindi pop-star Sarina Devan lives a wealthy ...	A singer finds out she was adopted when the ed...	NaN	Ameesha Patel|Jimmy Sheirgill|Nafisa Ali|Gulsh...	NaN	16 November 2001 (India)
1626	Sabse Bada Sukh	tt0069204	NaN	https://en.wikipedia.org/wiki/Sabse_Bada_Sukh	Sabse Bada Sukh	Sabse Bada Sukh	0	2018	\N	Comedy|Drama	6.1	13	Village born Lalloo re-locates to Bombay and ...	Village born Lalloo re-locates to Bombay and ...	NaN	Vijay Arora|Asrani|Rajni Bala|Kumud Damle|Utpa...	NaN	NaN
1627	Daaka	tt10833860	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Daaka	Daaka	Daaka	0	2019	136	Action	7.4	38	Shinda tries robbing a bank so he can be wealt...	Shinda tries robbing a bank so he can be wealt...	NaN	Gippy Grewal|Zareen Khan|	NaN	1 November 2019 (USA)
1628	Humsafar	tt2403201	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Humsafar	Humsafar	Humsafar	0	2011	35	Drama|Romance	9.0	2968	Sara and Ashar are childhood friends who share...	Ashar and Khirad are forced to get married due...	NaN	Fawad Khan|	NaN	TV Series (2011–2012)
1629 rows × 18 columns


ipl = pd.read_csv('ipl-matches.csv')
ipl
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	SuperOver	WinningTeam	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2
0	1312200	Ahmedabad	2022-05-29	2022	Final	Rajasthan Royals	Gujarat Titans	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	bat	N	Gujarat Titans	Wickets	7.0	NaN	HH Pandya	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pan...	CB Gaffaney	Nitin Menon
1	1312199	Ahmedabad	2022-05-27	2022	Qualifier 2	Royal Challengers Bangalore	Rajasthan Royals	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	field	N	Rajasthan Royals	Wickets	7.0	NaN	JC Buttler	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	CB Gaffaney	Nitin Menon
2	1312198	Kolkata	2022-05-25	2022	Eliminator	Royal Challengers Bangalore	Lucknow Super Giants	Eden Gardens, Kolkata	Lucknow Super Giants	field	N	Royal Challengers Bangalore	Runs	14.0	NaN	RM Patidar	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...	['Q de Kock', 'KL Rahul', 'M Vohra', 'DJ Hooda...	J Madanagopal	MA Gough
3	1312197	Kolkata	2022-05-24	2022	Qualifier 1	Rajasthan Royals	Gujarat Titans	Eden Gardens, Kolkata	Gujarat Titans	field	N	Gujarat Titans	Wickets	7.0	NaN	DA Miller	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pan...	BNJ Oxenford	VK Sharma
4	1304116	Mumbai	2022-05-22	2022	70	Sunrisers Hyderabad	Punjab Kings	Wankhede Stadium, Mumbai	Sunrisers Hyderabad	bat	N	Punjab Kings	Wickets	5.0	NaN	Harpreet Brar	['PK Garg', 'Abhishek Sharma', 'RA Tripathi', ...	['JM Bairstow', 'S Dhawan', 'M Shahrukh Khan',...	AK Chaudhary	NA Patwardhan
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
945	335986	Kolkata	2008-04-20	2007/08	4	Kolkata Knight Riders	Deccan Chargers	Eden Gardens	Deccan Chargers	bat	N	Kolkata Knight Riders	Wickets	5.0	NaN	DJ Hussey	['WP Saha', 'BB McCullum', 'RT Ponting', 'SC G...	['AC Gilchrist', 'Y Venugopal Rao', 'VVS Laxma...	BF Bowden	K Hariharan
946	335985	Mumbai	2008-04-20	2007/08	5	Mumbai Indians	Royal Challengers Bangalore	Wankhede Stadium	Mumbai Indians	bat	N	Royal Challengers Bangalore	Wickets	5.0	NaN	MV Boucher	['L Ronchi', 'ST Jayasuriya', 'DJ Thornely', '...	['S Chanderpaul', 'R Dravid', 'LRPL Taylor', '...	SJ Davis	DJ Harper
947	335984	Delhi	2008-04-19	2007/08	3	Delhi Daredevils	Rajasthan Royals	Feroz Shah Kotla	Rajasthan Royals	bat	N	Delhi Daredevils	Wickets	9.0	NaN	MF Maharoof	['G Gambhir', 'V Sehwag', 'S Dhawan', 'MK Tiwa...	['T Kohli', 'YK Pathan', 'SR Watson', 'M Kaif'...	Aleem Dar	GA Pratapkumar
948	335983	Chandigarh	2008-04-19	2007/08	2	Kings XI Punjab	Chennai Super Kings	Punjab Cricket Association Stadium, Mohali	Chennai Super Kings	bat	N	Chennai Super Kings	Runs	33.0	NaN	MEK Hussey	['K Goel', 'JR Hopes', 'KC Sangakkara', 'Yuvra...	['PA Patel', 'ML Hayden', 'MEK Hussey', 'MS Dh...	MR Benson	SL Shastri
949	335982	Bangalore	2008-04-18	2007/08	1	Royal Challengers Bangalore	Kolkata Knight Riders	M Chinnaswamy Stadium	Royal Challengers Bangalore	field	N	Kolkata Knight Riders	Runs	140.0	NaN	BB McCullum	['R Dravid', 'W Jaffer', 'V Kohli', 'JH Kallis...	['SC Ganguly', 'BB McCullum', 'RT Ponting', 'D...	Asad Rauf	RE Koertzen
950 rows × 20 columns

DataFrame Attributes and Methods

# shape
movies.shape
ipl.shape
     
(950, 20)

# dtypes
movies.dtypes
ipl.dtypes
     
ID                   int64
City                object
Date                object
Season              object
MatchNumber         object
Team1               object
Team2               object
Venue               object
TossWinner          object
TossDecision        object
SuperOver           object
WinningTeam         object
WonBy               object
Margin             float64
method              object
Player_of_Match     object
Team1Players        object
Team2Players        object
Umpire1             object
Umpire2             object
dtype: object

# index
movies.index
ipl.index
     
RangeIndex(start=0, stop=950, step=1)

# columns
movies.columns
ipl.columns
student.columns
     
Index(['iq', 'marks', 'package'], dtype='object')

# values
student.values
ipl.values
     
array([[1312200, 'Ahmedabad', '2022-05-29', ...,
        "['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pandya', 'DA Miller', 'R Tewatia', 'Rashid Khan', 'R Sai Kishore', 'LH Ferguson', 'Yash Dayal', 'Mohammed Shami']",
        'CB Gaffaney', 'Nitin Menon'],
       [1312199, 'Ahmedabad', '2022-05-27', ...,
        "['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D Padikkal', 'SO Hetmyer', 'R Parag', 'R Ashwin', 'TA Boult', 'YS Chahal', 'M Prasidh Krishna', 'OC McCoy']",
        'CB Gaffaney', 'Nitin Menon'],
       [1312198, 'Kolkata', '2022-05-25', ...,
        "['Q de Kock', 'KL Rahul', 'M Vohra', 'DJ Hooda', 'MP Stoinis', 'E Lewis', 'KH Pandya', 'PVD Chameera', 'Mohsin Khan', 'Avesh Khan', 'Ravi Bishnoi']",
        'J Madanagopal', 'MA Gough'],
       ...,
       [335984, 'Delhi', '2008-04-19', ...,
        "['T Kohli', 'YK Pathan', 'SR Watson', 'M Kaif', 'DS Lehmann', 'RA Jadeja', 'M Rawat', 'D Salunkhe', 'SK Warne', 'SK Trivedi', 'MM Patel']",
        'Aleem Dar', 'GA Pratapkumar'],
       [335983, 'Chandigarh', '2008-04-19', ...,
        "['PA Patel', 'ML Hayden', 'MEK Hussey', 'MS Dhoni', 'SK Raina', 'JDP Oram', 'S Badrinath', 'Joginder Sharma', 'P Amarnath', 'MS Gony', 'M Muralitharan']",
        'MR Benson', 'SL Shastri'],
       [335982, 'Bangalore', '2008-04-18', ...,
        "['SC Ganguly', 'BB McCullum', 'RT Ponting', 'DJ Hussey', 'Mohammad Hafeez', 'LR Shukla', 'WP Saha', 'AB Agarkar', 'AB Dinda', 'M Kartik', 'I Sharma']",
        'Asad Rauf', 'RE Koertzen']], dtype=object)

# head and tail
movies.head(2)
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)

ipl.tail(2)
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	SuperOver	WinningTeam	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2
948	335983	Chandigarh	2008-04-19	2007/08	2	Kings XI Punjab	Chennai Super Kings	Punjab Cricket Association Stadium, Mohali	Chennai Super Kings	bat	N	Chennai Super Kings	Runs	33.0	NaN	MEK Hussey	['K Goel', 'JR Hopes', 'KC Sangakkara', 'Yuvra...	['PA Patel', 'ML Hayden', 'MEK Hussey', 'MS Dh...	MR Benson	SL Shastri
949	335982	Bangalore	2008-04-18	2007/08	1	Royal Challengers Bangalore	Kolkata Knight Riders	M Chinnaswamy Stadium	Royal Challengers Bangalore	field	N	Kolkata Knight Riders	Runs	140.0	NaN	BB McCullum	['R Dravid', 'W Jaffer', 'V Kohli', 'JH Kallis...	['SC Ganguly', 'BB McCullum', 'RT Ponting', 'D...	Asad Rauf	RE Koertzen

# sample
ipl.sample(5)
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	SuperOver	WinningTeam	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2
336	1082628	Mumbai	2017-05-01	2017	38	Mumbai Indians	Royal Challengers Bangalore	Wankhede Stadium	Royal Challengers Bangalore	bat	N	Mumbai Indians	Wickets	5.0	NaN	RG Sharma	['PA Patel', 'JC Buttler', 'N Rana', 'RG Sharm...	['V Kohli', 'Mandeep Singh', 'TM Head', 'AB de...	AK Chaudhary	CB Gaffaney
98	1254107	Sharjah	2021-09-25	2021	37	Punjab Kings	Sunrisers Hyderabad	Sharjah Cricket Stadium	Sunrisers Hyderabad	field	N	Punjab Kings	Runs	5.0	NaN	JO Holder	['KL Rahul', 'MA Agarwal', 'CH Gayle', 'AK Mar...	['DA Warner', 'WP Saha', 'KS Williamson', 'MK ...	RK Illingworth	YC Barde
890	392182	Cape Town	2009-04-18	2009	2	Royal Challengers Bangalore	Rajasthan Royals	Newlands	Royal Challengers Bangalore	bat	N	Royal Challengers Bangalore	Runs	75.0	NaN	R Dravid	['JD Ryder', 'RV Uthappa', 'LRPL Taylor', 'KP ...	['GC Smith', 'SA Asnodkar', 'NK Patel', 'T Hen...	BR Doctrove	RB Tiffin
157	1216533	Abu Dhabi	2020-10-19	2020/21	37	Chennai Super Kings	Rajasthan Royals	Sheikh Zayed Stadium	Chennai Super Kings	bat	N	Rajasthan Royals	Wickets	7.0	NaN	JC Buttler	['SM Curran', 'F du Plessis', 'SR Watson', 'AT...	['BA Stokes', 'RV Uthappa', 'SV Samson', 'SPD ...	CB Gaffaney	VK Sharma
386	980991	Chandigarh	2016-05-15	2016	46	Kings XI Punjab	Sunrisers Hyderabad	Punjab Cricket Association IS Bindra Stadium, ...	Kings XI Punjab	bat	N	Sunrisers Hyderabad	Wickets	7.0	NaN	HM Amla	['HM Amla', 'M Vijay', 'WP Saha', 'Gurkeerat S...	['DA Warner', 'S Dhawan', 'DJ Hooda', 'Yuvraj ...	KN Ananthapadmanabhan	M Erasmus

# info
movies.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 1629 entries, 0 to 1628
Data columns (total 18 columns):
 #   Column            Non-Null Count  Dtype  
---  ------            --------------  -----  
 0   title_x           1629 non-null   object 
 1   imdb_id           1629 non-null   object 
 2   poster_path       1526 non-null   object 
 3   wiki_link         1629 non-null   object 
 4   title_y           1629 non-null   object 
 5   original_title    1629 non-null   object 
 6   is_adult          1629 non-null   int64  
 7   year_of_release   1629 non-null   int64  
 8   runtime           1629 non-null   object 
 9   genres            1629 non-null   object 
 10  imdb_rating       1629 non-null   float64
 11  imdb_votes        1629 non-null   int64  
 12  story             1609 non-null   object 
 13  summary           1629 non-null   object 
 14  tagline           557 non-null    object 
 15  actors            1624 non-null   object 
 16  wins_nominations  707 non-null    object 
 17  release_date      1522 non-null   object 
dtypes: float64(1), int64(3), object(14)
memory usage: 229.2+ KB

ipl.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 950 entries, 0 to 949
Data columns (total 20 columns):
 #   Column           Non-Null Count  Dtype  
---  ------           --------------  -----  
 0   ID               950 non-null    int64  
 1   City             899 non-null    object 
 2   Date             950 non-null    object 
 3   Season           950 non-null    object 
 4   MatchNumber      950 non-null    object 
 5   Team1            950 non-null    object 
 6   Team2            950 non-null    object 
 7   Venue            950 non-null    object 
 8   TossWinner       950 non-null    object 
 9   TossDecision     950 non-null    object 
 10  SuperOver        946 non-null    object 
 11  WinningTeam      946 non-null    object 
 12  WonBy            950 non-null    object 
 13  Margin           932 non-null    float64
 14  method           19 non-null     object 
 15  Player_of_Match  946 non-null    object 
 16  Team1Players     950 non-null    object 
 17  Team2Players     950 non-null    object 
 18  Umpire1          950 non-null    object 
 19  Umpire2          950 non-null    object 
dtypes: float64(1), int64(1), object(18)
memory usage: 148.6+ KB

# describe
movies.describe()
     
is_adult	year_of_release	imdb_rating	imdb_votes
count	1629.0	1629.000000	1629.000000	1629.000000
mean	0.0	2010.263966	5.557459	5384.263352
std	0.0	5.381542	1.567609	14552.103231
min	0.0	2001.000000	0.000000	0.000000
25%	0.0	2005.000000	4.400000	233.000000
50%	0.0	2011.000000	5.600000	1000.000000
75%	0.0	2015.000000	6.800000	4287.000000
max	0.0	2019.000000	9.400000	310481.000000

ipl.describe()
     
ID	Margin
count	9.500000e+02	932.000000
mean	8.304852e+05	17.056867
std	3.375678e+05	21.633109
min	3.359820e+05	1.000000
25%	5.012612e+05	6.000000
50%	8.297380e+05	8.000000
75%	1.175372e+06	19.000000
max	1.312200e+06	146.000000

# isnull
movies.isnull().sum()
     
title_x                0
imdb_id                0
poster_path          103
wiki_link              0
title_y                0
original_title         0
is_adult               0
year_of_release        0
runtime                0
genres                 0
imdb_rating            0
imdb_votes             0
story                 20
summary                0
tagline             1072
actors                 5
wins_nominations     922
release_date         107
dtype: int64

# duplicated
movies.duplicated().sum()
     
0

students.duplicated().sum()
     
1

# rename
students
     
iq	percent	lpa
0	100	80	10
1	90	70	7
2	120	100	14
3	80	50	2
4	0	0	0
5	0	0	0

students.rename(columns={'marks':'percent','package':'lpa'},inplace=True)
     
Math Methods

# sum -> axis argument
students.sum(axis=0)
     
iq         390
percent    300
lpa         33
dtype: int64

students.mean(axis=1)
     
0    63.333333
1    55.666667
2    78.000000
3    44.000000
4     0.000000
5     0.000000
dtype: float64

students.var()
     
iq         2710.0
percent    1760.0
lpa          33.5
dtype: float64


     
Selecting cols from a DataFrame

# single cols
movies['title_x']
     
0                   Uri: The Surgical Strike
1                              Battalion 609
2       The Accidental Prime Minister (film)
3                            Why Cheat India
4                            Evening Shadows
                        ...                 
1624                   Tera Mera Saath Rahen
1625                    Yeh Zindagi Ka Safar
1626                         Sabse Bada Sukh
1627                                   Daaka
1628                                Humsafar
Name: title_x, Length: 1629, dtype: object

ipl['Venue']
     
0                Narendra Modi Stadium, Ahmedabad
1                Narendra Modi Stadium, Ahmedabad
2                           Eden Gardens, Kolkata
3                           Eden Gardens, Kolkata
4                        Wankhede Stadium, Mumbai
                          ...                    
945                                  Eden Gardens
946                              Wankhede Stadium
947                              Feroz Shah Kotla
948    Punjab Cricket Association Stadium, Mohali
949                         M Chinnaswamy Stadium
Name: Venue, Length: 950, dtype: object

# multiple cols
movies[['year_of_release','actors','title_x']]
     
year_of_release	actors	title_x
0	2019	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	Uri: The Surgical Strike
1	2019	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	Battalion 609
2	2019	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	The Accidental Prime Minister (film)
3	2019	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	Why Cheat India
4	2018	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	Evening Shadows
...	...	...	...
1624	2001	Ajay Devgn|Sonali Bendre|Namrata Shirodkar|Pre...	Tera Mera Saath Rahen
1625	2001	Ameesha Patel|Jimmy Sheirgill|Nafisa Ali|Gulsh...	Yeh Zindagi Ka Safar
1626	2018	Vijay Arora|Asrani|Rajni Bala|Kumud Damle|Utpa...	Sabse Bada Sukh
1627	2019	Gippy Grewal|Zareen Khan|	Daaka
1628	2011	Fawad Khan|	Humsafar
1629 rows × 3 columns


ipl[['Team1','Team2','WinningTeam']]
     
Team1	Team2	WinningTeam
0	Rajasthan Royals	Gujarat Titans	Gujarat Titans
1	Royal Challengers Bangalore	Rajasthan Royals	Rajasthan Royals
2	Royal Challengers Bangalore	Lucknow Super Giants	Royal Challengers Bangalore
3	Rajasthan Royals	Gujarat Titans	Gujarat Titans
4	Sunrisers Hyderabad	Punjab Kings	Punjab Kings
...	...	...	...
945	Kolkata Knight Riders	Deccan Chargers	Kolkata Knight Riders
946	Mumbai Indians	Royal Challengers Bangalore	Royal Challengers Bangalore
947	Delhi Daredevils	Rajasthan Royals	Delhi Daredevils
948	Kings XI Punjab	Chennai Super Kings	Chennai Super Kings
949	Royal Challengers Bangalore	Kolkata Knight Riders	Kolkata Knight Riders
950 rows × 3 columns

Selecting rows from a DataFrame
iloc - searches using index positions
loc - searches using index labels

# single row
movies.iloc[5]
     
title_x                                                   Soni (film)
imdb_id                                                     tt6078866
poster_path         https://upload.wikimedia.org/wikipedia/en/thum...
wiki_link                   https://en.wikipedia.org/wiki/Soni_(film)
title_y                                                          Soni
original_title                                                   Soni
is_adult                                                            0
year_of_release                                                  2018
runtime                                                            97
genres                                                          Drama
imdb_rating                                                       7.2
imdb_votes                                                       1595
story               Soni  a young policewoman in Delhi  and her su...
summary             While fighting crimes against women in Delhi  ...
tagline                                                           NaN
actors              Geetika Vidya Ohlyan|Saloni Batra|Vikas Shukla...
wins_nominations                               3 wins & 5 nominations
release_date                                    18 January 2019 (USA)
Name: 5, dtype: object

# multiple row
movies.iloc[:5]
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)
3	Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)
4	Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)

# fancy indexing
movies.iloc[[0,4,5]]
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
4	Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)
5	Soni (film)	tt6078866	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Soni_(film)	Soni	Soni	0	2018	97	Drama	7.2	1595	Soni a young policewoman in Delhi and her su...	While fighting crimes against women in Delhi ...	NaN	Geetika Vidya Ohlyan|Saloni Batra|Vikas Shukla...	3 wins & 5 nominations	18 January 2019 (USA)

# loc
students
     
iq	marks	package
name			
nitish	100	80	10
ankit	90	70	7
rupesh	120	100	14
rishabh	80	50	2
amit	0	0	0
ankita	0	0	0

students.loc['nitish']
     
iq         100
marks       80
package     10
Name: nitish, dtype: int64

students.loc['nitish':'rishabh':2]
     
iq	marks	package
name			
nitish	100	80	10
rupesh	120	100	14

students.loc[['nitish','ankita','rupesh']]
     
iq	marks	package
name			
nitish	100	80	10
ankita	0	0	0
rupesh	120	100	14

students.iloc[[0,3,4]]
     
iq	marks	package
name			
nitish	100	80	10
rishabh	80	50	2
amit	0	0	0
Selecting both rows and cols

movies.iloc[0:3,0:3]
     
title_x	imdb_id	poster_path
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...
1	Battalion 609	tt9472208	NaN
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...

movies.loc[0:2,'title_x':'poster_path']
     
title_x	imdb_id	poster_path
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...
1	Battalion 609	tt9472208	NaN
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...
Filtering a DataFrame

ipl.head(2)
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	SuperOver	WinningTeam	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2
0	1312200	Ahmedabad	2022-05-29	2022	Final	Rajasthan Royals	Gujarat Titans	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	bat	N	Gujarat Titans	Wickets	7.0	NaN	HH Pandya	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pan...	CB Gaffaney	Nitin Menon
1	1312199	Ahmedabad	2022-05-27	2022	Qualifier 2	Royal Challengers Bangalore	Rajasthan Royals	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	field	N	Rajasthan Royals	Wickets	7.0	NaN	JC Buttler	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	CB Gaffaney	Nitin Menon

# find all the final winners
mask = ipl['MatchNumber'] == 'Final'
new_df = ipl[mask]
new_df[['Season','WinningTeam']]

ipl[ipl['MatchNumber'] == 'Final'][['Season','WinningTeam']]
     
Season	WinningTeam
0	2022	Gujarat Titans
74	2021	Chennai Super Kings
134	2020/21	Mumbai Indians
194	2019	Mumbai Indians
254	2018	Chennai Super Kings
314	2017	Mumbai Indians
373	2016	Sunrisers Hyderabad
433	2015	Mumbai Indians
492	2014	Kolkata Knight Riders
552	2013	Mumbai Indians
628	2012	Kolkata Knight Riders
702	2011	Chennai Super Kings
775	2009/10	Chennai Super Kings
835	2009	Deccan Chargers
892	2007/08	Rajasthan Royals

# how many super over finishes have occured
ipl[ipl['SuperOver'] == 'Y'].shape[0]
     
14

# how many matches has csk won in kolkata
ipl[(ipl['City'] == 'Kolkata') & (ipl['WinningTeam'] == 'Chennai Super Kings')].shape[0]
     
5

# toss winner is match winner in percentage
(ipl[ipl['TossWinner'] == ipl['WinningTeam']].shape[0]/ipl.shape[0])*100
     
51.473684210526315

# movies with rating higher than 8 and votes>10000
movies[(movies['imdb_rating'] > 8.5) & (movies['imdb_votes'] > 10000)].shape[0]
     
0

# Action movies with rating higher than 7.5
# mask1 = movies['genres'].str.split('|').apply(lambda x:'Action' in x)
mask1 = movies['genres'].str.contains('Action')
mask2 = movies['imdb_rating'] > 7.5

movies[mask1 & mask2]
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
41	Family of Thakurganj	tt8897986	https://upload.wikimedia.org/wikipedia/en/9/99...	https://en.wikipedia.org/wiki/Family_of_Thakur...	Family of Thakurganj	Family of Thakurganj	0	2019	127	Action|Drama	9.4	895	The film is based on small town of North India...	The film is based on small town of North India...	NaN	Jimmy Sheirgill|Mahie Gill|Nandish Singh|Prana...	NaN	19 July 2019 (India)
84	Mukkabaaz	tt7180544	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Mukkabaaz	The Brawler	Mukkabaaz	0	2017	154	Action|Drama|Sport	8.1	5434	A boxer (Shravan) belonging to upper cast tra...	A boxer struggles to make his mark in the boxi...	NaN	Viineet Kumar|Jimmy Sheirgill|Zoya Hussain|Rav...	3 wins & 6 nominations	12 January 2018 (USA)
106	Raazi	tt7098658	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Raazi	Raazi	Raazi	0	2018	138	Action|Drama|Thriller	7.8	20289	Hidayat Khan is the son of an Indian freedom f...	A Kashmiri woman agrees to marry a Pakistani a...	An incredible true story	Alia Bhatt|Vicky Kaushal|Rajit Kapoor|Shishir ...	21 wins & 26 nominations	11 May 2018 (USA)
110	Parmanu: The Story of Pokhran	tt6826438	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Parmanu:_The_Sto...	Parmanu: The Story of Pokhran	Parmanu: The Story of Pokhran	0	2018	129	Action|Drama|History	7.7	18292	Captain Ashwat Raina's efforts to turn India i...	Ashwat Raina and his teammates arrive in Pokhr...	1998| India: one secret operation| six Indians...	John Abraham|Boman Irani|Diana Penty|Anuja Sat...	NaN	25 May 2018 (USA)
112	Bhavesh Joshi Superhero	tt6129302	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Bhavesh_Joshi_Su...	Bhavesh Joshi Superhero	Bhavesh Joshi Superhero	0	2018	154	Action|Drama	7.6	4928	Bhavesh Joshi Superhero is an action film abou...	The origin story of Bhavesh Joshi an Indian s...	This year| justice will have a new name.	Harshvardhan Kapoor|Priyanshu Painyuli|Ashish ...	2 nominations	1 June 2018 (USA)
169	The Ghazi Attack	tt6299040	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Ghazi_Attack...	The Ghazi Attack	The Ghazi Attack	0	2017	116	Action|Thriller|War	7.6	10332	In 1971 amid rising tensions between India an...	A Pakistani submarine Ghazi plans to secretly...	The war you did not know about	Rana Daggubati|Kay Kay Menon|Atul Kulkarni|Om ...	1 win & 7 nominations	17 February 2017 (USA)
219	Raag Desh (film)	tt6080746	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Raagdesh	Raag Desh	Raag Desh	0	2017	135	Action|Drama|History	8.3	341	A period film based on the historic 1945 India...	A period film based on the historic 1945 India...	NaN	Kunal Kapoor|Amit Sadh|Mohit Marwah|Kenneth De...	NaN	28 July 2017 (India)
258	Irudhi Suttru	tt5310090	https://upload.wikimedia.org/wikipedia/en/f/fe...	https://en.wikipedia.org/wiki/Saala_Khadoos	Saala Khadoos	Saala Khadoos	0	2016	109	Action|Drama|Sport	7.6	10507	An under-fire boxing coach Prabhu is transfer...	The story of a former boxer who quits boxing f...	NaN	Madhavan|Ritika Singh|Mumtaz Sorcar|Nassar|Rad...	9 wins & 2 nominations	29 January 2016 (USA)
280	Laal Rang	tt5600714	NaN	https://en.wikipedia.org/wiki/Laal_Rang	Laal Rang	Laal Rang	0	2016	147	Action|Crime|Drama	8.0	3741	The friendship of two men is tested when thing...	The friendship of two men is tested when thing...	Every job good or bad| must be done with honesty.	Randeep Hooda|Akshay Oberoi|Rajniesh Duggall|P...	NaN	22 April 2016 (India)
297	Udta Punjab	tt4434004	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Udta_Punjab	Udta Punjab	Udta Punjab	0	2016	148	Action|Crime|Drama	7.8	23995	What on earth can a rock star a migrant labor...	A story that revolves around drug abuse in the...	NaN	Shahid Kapoor|Alia Bhatt|Kareena Kapoor|Diljit...	11 wins & 19 nominations	17 June 2016 (USA)
354	Dangal (film)	tt5074352	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Dangal_(film)	Dangal	Dangal	0	2016	161	Action|Biography|Drama	8.4	131338	Biopic of Mahavir Singh Phogat who taught wre...	Former wrestler Mahavir Singh Phogat and his t...	You think our girls are any lesser than boys?	Aamir Khan|Fatima Sana Shaikh|Sanya Malhotra|S...	23 wins & 4 nominations	21 December 2016 (USA)
362	Bajrangi Bhaijaan	tt3863552	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Bajrangi_Bhaijaan	Bajrangi Bhaijaan	Bajrangi Bhaijaan	0	2015	163	Action|Comedy|Drama	8.0	65877	A little mute girl from a Pakistani village ge...	An Indian man with a magnanimous heart takes a...	NaN	Salman Khan|Harshaali Malhotra|Nawazuddin Sidd...	25 wins & 13 nominations	17 July 2015 (USA)
365	Baby (2015 Hindi film)	tt3848892	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Baby_(2015_Hindi...	Baby	Baby	0	2015	159	Action|Thriller	8.0	49426	The country is perpetually under threat from t...	An elite counter-intelligence unit learns of a...	History Is Made By Those Who Give A Damn!	Akshay Kumar|Danny Denzongpa|Rana Daggubati|Ta...	1 win	23 January 2015 (India)
393	Detective Byomkesh Bakshy!	tt3447364	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Detective_Byomke...	Detective Byomkesh Bakshy!	Detective Byomkesh Bakshy!	0	2015	139	Action|Mystery|Thriller	7.6	14674	CALCUTTA 1943 A WAR - A MYSTERY - and A DETECT...	While investigating the disappearance of a che...	Expect The Unexpected	Sushant Singh Rajput|Anand Tiwari|Neeraj Kabi|...	NaN	3 April 2015 (USA)
449	Titli (2014 film)	tt3019620	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Titli_(2014_film)	Titli	Titli	0	2014	116	Action|Drama|Thriller	7.6	3677	In the badlands of Delhi's dystopic underbelly...	A Hindi feature film set in the lower depths o...	Daring| Desireable| Dangerous	Nawazuddin Siddiqui|Niharika Singh|Anil George...	4 wins & 5 nominations	20 June 2014 (USA)
536	Haider (film)	tt3390572	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Haider_(film)	Haider	Haider	0	2014	160	Action|Crime|Drama	8.1	46912	Vishal Bhardwaj's adaptation of William Shakes...	A young man returns to Kashmir after his fathe...	NaN	Tabu|Shahid Kapoor|Shraddha Kapoor|Kay Kay Men...	28 wins & 24 nominations	2 October 2014 (USA)
589	Vishwaroopam	tt2199711	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Vishwaroop_(Hind...	Vishwaroopam	Vishwaroopam	0	2013	148	Action|Thriller	8.2	38016	Vishwanathan a Kathak dance teacher in New Yo...	When a classical dancer's suspecting wife sets...	NaN	Kamal Haasan|Rahul Bose|Shekhar Kapur|Pooja Ku...	5 wins & 11 nominations	25 January 2013 (India)
625	Madras Cafe	tt2855648	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Madras_Cafe	Madras Cafe	Madras Cafe	0	2013	130	Action|Drama|Thriller	7.7	21393	An Indian Intelligence agent (portrayed by Joh...	An Indian intelligence agent journeys to a war...	NaN	John Abraham|Nargis Fakhri|Raashi Khanna|Praka...	10 wins & 10 nominations	23 August 2013 (India)
668	Paan Singh Tomar (film)	tt1620933	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Paan_Singh_Tomar...	Paan Singh Tomar	Paan Singh Tomar	0	2012	135	Action|Biography|Crime	8.2	29994	Paan Singh Tomar is a Hindi-language film bas...	The story of Paan Singh Tomar an Indian athle...	NaN	Irrfan Khan|	10 wins & 11 nominations	2 March 2012 (USA)
693	Gangs of Wasseypur	tt1954470	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Gangs_of_Wasseypur	Gangs of Wasseypur	Gangs of Wasseypur	0	2012	321	Action|Comedy|Crime	8.2	71636	Shahid Khan is exiled after impersonating the ...	A clash between Sultan and Shahid Khan leads t...	NaN	Manoj Bajpayee|Richa Chadha|Nawazuddin Siddiqu...	12 wins & 43 nominations	2 August 2012 (Singapore)
694	Gangs of Wasseypur – Part 2	tt1954470	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Gangs_of_Wasseyp...	Gangs of Wasseypur	Gangs of Wasseypur	0	2012	321	Action|Comedy|Crime	8.2	71636	Shahid Khan is exiled after impersonating the ...	A clash between Sultan and Shahid Khan leads t...	NaN	Manoj Bajpayee|Richa Chadha|Nawazuddin Siddiqu...	12 wins & 43 nominations	2 August 2012 (Singapore)
982	Jodhaa Akbar	tt0449994	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Jodhaa_Akbar	Jodhaa Akbar	Jodhaa Akbar	0	2008	213	Action|Drama|History	7.6	27541	Jodhaa Akbar is a sixteenth century love story...	A sixteenth century love story about a marriag...	NaN	Hrithik Roshan|Aishwarya Rai Bachchan|Sonu Soo...	32 wins & 21 nominations	15 February 2008 (USA)
1039	1971 (2007 film)	tt0983990	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/1971_(2007_film)	1971	1971	0	2007	160	Action|Drama|War	7.9	1121	Based on true facts the film revolves around ...	Based on true facts the film revolves around ...	Honor the heroes.......	Manoj Bajpayee|Ravi Kishan|Deepak Dobriyal|	1 win	9 March 2007 (India)
1058	Black Friday (2007 film)	tt0400234	https://upload.wikimedia.org/wikipedia/en/5/58...	https://en.wikipedia.org/wiki/Black_Friday_(20...	Black Friday	Black Friday	0	2004	143	Action|Crime|Drama	8.5	16761	A dramatic presentation of the bomb blasts tha...	Black Friday is a film about the investigation...	The story of the Bombay bomb blasts	Kay Kay Menon|Pavan Malhotra|Aditya Srivastava...	3 nominations	9 February 2007 (India)
1188	Omkara (2006 film)	tt0488414	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Omkara_(2006_film)	Omkara	Omkara	0	2006	155	Action|Crime|Drama	8.1	17594	Advocate Raghunath Mishra has arranged the mar...	A politically-minded enforcer's misguided trus...	NaN	Ajay Devgn|Saif Ali Khan|Vivek Oberoi|Kareena ...	19 wins & 20 nominations	28 July 2006 (USA)
1293	Sarkar (2005 film)	tt0432047	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Sarkar_(2005_film)	Sarkar	Sarkar	0	2005	124	Action|Crime|Drama	7.6	14694	Meet Subhash Nagre - a wealthy and influential...	The authority of a man who runs a parallel go...	'There are no Rights and Wrongs. Only Power' -...	Amitabh Bachchan|Abhishek Bachchan|Kay Kay Men...	2 wins & 10 nominations	1 July 2005 (India)
1294	Sehar	tt0477857	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Sehar	Sehar	Sehar	0	2005	125	Action|Crime|Drama	7.8	1861	At the tender age of 8 Ajay Kumar is traumatiz...	Ajay Kumar the newly appointed honest SSP of ...	NaN	Arshad Warsi|Pankaj Kapur|Mahima Chaudhry|Sush...	NaN	29 July 2005 (India)
1361	Lakshya (film)	tt0323013	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Lakshya_(film)	Lakshya	Lakshya	0	2004	186	Action|Drama|Romance	7.9	18777	Karan is a lazy good-for-nothing who lives on ...	An aimless jobless irresponsible grown man j...	It took him 24 years and 18000 feet to find hi...	Hrithik Roshan|Preity Zinta|Amitabh Bachchan|O...	4 wins & 10 nominations	18 June 2004 (USA)
1432	Gangaajal	tt0373856	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Gangaajal	Gangaajal	Gangaajal	0	2003	157	Action|Crime|Drama	7.8	14295	An SP Amit Kumar who is given charge of Tezpur...	An IPS officer motivates and leads a dysfuncti...	NaN	Ajay Devgn|Gracy Singh|Mohan Joshi|Yashpal Sha...	4 wins & 29 nominations	29 August 2003 (India)
1495	Company (film)	tt0296574	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Company_(film)	Company	Company	0	2002	155	Action|Crime|Drama	8.0	13474	Mallik is a henchman of Aslam Bhai a Mumbai u...	A small-time gangster named Chandu teams up wi...	A law & order enterprise	Ajay Devgn|Mohanlal|Manisha Koirala|Seema Bisw...	16 wins & 9 nominations	15 April 2002 (India)
1554	The Legend of Bhagat Singh	tt0319736	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Legend_of_Bh...	The Legend of Bhagat Singh	The Legend of Bhagat Singh	0	2002	155	Action|Biography|Drama	8.1	13455	Bhagat was born in British India during the ye...	The story of a young revolutionary who raised ...	NaN	Ajay Devgn|Sushant Singh|D. Santosh|Akhilendra...	11 wins & 5 nominations	7 June 2002 (India)
1607	Nayak (2001 Hindi film)	tt0291376	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Nayak_(2001_Hind...	Nayak: The Real Hero	Nayak: The Real Hero	0	2001	187	Action|Drama|Thriller	7.8	12522	Employed as a camera-man at a popular televisi...	A man accepts a challenge by the chief ministe...	Fight the power	Anil Kapoor|Rani Mukerji|Amrish Puri|Johnny Le...	2 nominations	7 September 2001 (India)

# write a function that can return the track record of 2 teams against each other
     
Adding new cols

# completely new
movies['Country'] = 'India'
movies.head()
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date	Country
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)	India
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)	India
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)	India
3	Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)	India
4	Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)	India

# from existing ones
movies.dropna(inplace=True)
     

movies['lead actor'] = movies['actors'].str.split('|').apply(lambda x:x[0])
movies.head()
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date	Country	lead actor
11	Gully Boy	tt2395469	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Gully_Boy	Gully Boy	Gully Boy	0	2019	153	Drama|Music	8.2	22440	Gully Boy is a film about a 22-year-old boy "M...	A coming-of-age story based on the lives of st...	Apna Time Aayega!	Ranveer Singh|Alia Bhatt|Siddhant Chaturvedi|V...	6 wins & 3 nominations	14 February 2019 (USA)	India	Ranveer Singh
34	Yeh Hai India	tt5525846	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Hai_India	Yeh Hai India	Yeh Hai India	0	2017	128	Action|Adventure|Drama	5.7	169	Yeh Hai India follows the story of a 25 years...	Yeh Hai India follows the story of a 25 years...	A Film for Every Indian	Gavie Chahal|Mohan Agashe|Mohan Joshi|Lom Harsh|	2 wins & 1 nomination	24 May 2019 (India)	India	Gavie Chahal
37	Article 15 (film)	tt10324144	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Article_15_(film)	Article 15	Article 15	0	2019	130	Crime|Drama	8.3	13417	In the rural heartlands of India an upright p...	In the rural heartlands of India an upright p...	Farq Bahut Kar Liya| Ab Farq Laayenge.	Ayushmann Khurrana|Nassar|Manoj Pahwa|Kumud Mi...	1 win	28 June 2019 (USA)	India	Ayushmann Khurrana
87	Aiyaary	tt6774212	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Aiyaary	Aiyaary	Aiyaary	0	2018	157	Action|Thriller	5.2	3538	General Gurinder Singh comes with a proposal t...	After finding out about an illegal arms deal ...	The Ultimate Trickery	Sidharth Malhotra|Manoj Bajpayee|Rakul Preet S...	1 nomination	16 February 2018 (USA)	India	Sidharth Malhotra
96	Raid (2018 film)	tt7363076	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Raid_(2018_film)	Raid	Raid	0	2018	122	Action|Crime|Drama	7.4	13159	Set in the 80s in Uttar Pradesh India Raid i...	A fearless income tax officer raids the mansio...	Heroes don't always come in uniform	Ajay Devgn|Saurabh Shukla|Ileana D'Cruz|Amit S...	2 wins & 3 nominations	16 March 2018 (India)	India	Ajay Devgn

movies.info()
     
<class 'pandas.core.frame.DataFrame'>
Int64Index: 298 entries, 11 to 1623
Data columns (total 19 columns):
 #   Column            Non-Null Count  Dtype  
---  ------            --------------  -----  
 0   title_x           298 non-null    object 
 1   imdb_id           298 non-null    object 
 2   poster_path       298 non-null    object 
 3   wiki_link         298 non-null    object 
 4   title_y           298 non-null    object 
 5   original_title    298 non-null    object 
 6   is_adult          298 non-null    int64  
 7   year_of_release   298 non-null    int64  
 8   runtime           298 non-null    object 
 9   genres            298 non-null    object 
 10  imdb_rating       298 non-null    float64
 11  imdb_votes        298 non-null    int64  
 12  story             298 non-null    object 
 13  summary           298 non-null    object 
 14  tagline           298 non-null    object 
 15  actors            298 non-null    object 
 16  wins_nominations  298 non-null    object 
 17  release_date      298 non-null    object 
 18  Country           298 non-null    object 
dtypes: float64(1), int64(3), object(15)
memory usage: 46.6+ KB
Important DataFrame Functions

# astype
ipl.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 950 entries, 0 to 949
Data columns (total 20 columns):
 #   Column           Non-Null Count  Dtype  
---  ------           --------------  -----  
 0   ID               950 non-null    int64  
 1   City             899 non-null    object 
 2   Date             950 non-null    object 
 3   Season           950 non-null    object 
 4   MatchNumber      950 non-null    object 
 5   Team1            950 non-null    object 
 6   Team2            950 non-null    object 
 7   Venue            950 non-null    object 
 8   TossWinner       950 non-null    object 
 9   TossDecision     950 non-null    object 
 10  SuperOver        946 non-null    object 
 11  WinningTeam      946 non-null    object 
 12  WonBy            950 non-null    object 
 13  Margin           932 non-null    float64
 14  method           19 non-null     object 
 15  Player_of_Match  946 non-null    object 
 16  Team1Players     950 non-null    object 
 17  Team2Players     950 non-null    object 
 18  Umpire1          950 non-null    object 
 19  Umpire2          950 non-null    object 
dtypes: float64(1), int64(1), object(18)
memory usage: 148.6+ KB

ipl['ID'] = ipl['ID'].astype('int32')
     

ipl.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 950 entries, 0 to 949
Data columns (total 20 columns):
 #   Column           Non-Null Count  Dtype  
---  ------           --------------  -----  
 0   ID               950 non-null    int32  
 1   City             899 non-null    object 
 2   Date             950 non-null    object 
 3   Season           950 non-null    object 
 4   MatchNumber      950 non-null    object 
 5   Team1            950 non-null    object 
 6   Team2            950 non-null    object 
 7   Venue            950 non-null    object 
 8   TossWinner       950 non-null    object 
 9   TossDecision     950 non-null    object 
 10  SuperOver        946 non-null    object 
 11  WinningTeam      946 non-null    object 
 12  WonBy            950 non-null    object 
 13  Margin           932 non-null    float64
 14  method           19 non-null     object 
 15  Player_of_Match  946 non-null    object 
 16  Team1Players     950 non-null    object 
 17  Team2Players     950 non-null    object 
 18  Umpire1          950 non-null    object 
 19  Umpire2          950 non-null    object 
dtypes: float64(1), int32(1), object(18)
memory usage: 144.9+ KB

# ipl['Season'] = ipl['Season'].astype('category')
ipl['Team1'] = ipl['Team1'].astype('category')
ipl['Team2'] = ipl['Team2'].astype('category')
     

ipl.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 950 entries, 0 to 949
Data columns (total 20 columns):
 #   Column           Non-Null Count  Dtype   
---  ------           --------------  -----   
 0   ID               950 non-null    int32   
 1   City             899 non-null    object  
 2   Date             950 non-null    object  
 3   Season           950 non-null    category
 4   MatchNumber      950 non-null    object  
 5   Team1            950 non-null    category
 6   Team2            950 non-null    category
 7   Venue            950 non-null    object  
 8   TossWinner       950 non-null    object  
 9   TossDecision     950 non-null    object  
 10  SuperOver        946 non-null    object  
 11  WinningTeam      946 non-null    object  
 12  WonBy            950 non-null    object  
 13  Margin           932 non-null    float64 
 14  method           19 non-null     object  
 15  Player_of_Match  946 non-null    object  
 16  Team1Players     950 non-null    object  
 17  Team2Players     950 non-null    object  
 18  Umpire1          950 non-null    object  
 19  Umpire2          950 non-null    object  
dtypes: category(3), float64(1), int32(1), object(15)
memory usage: 127.4+ KB


     

# value_counts
     

# find which player has won most potm -> in finals and qualifiers
     

# Toss decision plot
     

# how many matches each team has played
     

# sort_values -> ascending -> na_position -> inplace -> multiple cols
     

#################################################################################



More Important Functions

# value_counts
# sort_values
# rank
# sort index
# set index
# rename index -> rename
# reset index
# unique & nunique
# isnull/notnull/hasnans
# dropna
# fillna
# drop_duplicates
# drop
# apply
# isin
# corr
# nlargest -> nsmallest
# insert
# copy
     

import numpy as np
import pandas as pd
     

a = pd.Series([1,1,1,2,2,3])
a.value_counts()
     
1    3
2    2
3    1
dtype: int64

# value_counts(series and dataframe)


marks = pd.DataFrame([
    [100,80,10],
    [90,70,7],
    [120,100,14],
    [80,70,14],
    [80,70,14]
],columns=['iq','marks','package'])

marks
     
iq	marks	package
0	100	80	10
1	90	70	7
2	120	100	14
3	80	70	14
4	80	70	14

marks.value_counts()
     
iq   marks  package
80   70     14         2
90   70     7          1
100  80     10         1
120  100    14         1
dtype: int64

ipl = pd.read_csv('ipl-matches.csv')
ipl[~ipl['MatchNumber'].str.isdigit()]['Player_of_Match'].value_counts()
     
KA Pollard           3
F du Plessis         3
SK Raina             3
A Kumble             2
MK Pandey            2
YK Pathan            2
M Vijay              2
JJ Bumrah            2
AB de Villiers       2
SR Watson            2
HH Pandya            1
Harbhajan Singh      1
A Nehra              1
V Sehwag             1
UT Yadav             1
MS Bisla             1
BJ Hodge             1
MEK Hussey           1
MS Dhoni             1
CH Gayle             1
MM Patel             1
DE Bollinger         1
AC Gilchrist         1
RG Sharma            1
DA Warner            1
MC Henriques         1
JC Buttler           1
RM Patidar           1
DA Miller            1
VR Iyer              1
SP Narine            1
RD Gaikwad           1
TA Boult             1
MP Stoinis           1
KS Williamson        1
RR Pant              1
SA Yadav             1
Rashid Khan          1
AD Russell           1
KH Pandya            1
KV Sharma            1
NM Coulter-Nile      1
Washington Sundar    1
BCJ Cutting          1
M Ntini              1
Name: Player_of_Match, dtype: int64

# find which player has won most potm -> in finals and qualifiers
     

# Toss decision plot
ipl['TossDecision'].value_counts().plot(kind='pie')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f034efd49d0>


# how many matches each team has played
(ipl['Team2'].value_counts() + ipl['Team1'].value_counts()).sort_values(ascending=False)
     
Mumbai Indians                 231
Royal Challengers Bangalore    226
Kolkata Knight Riders          223
Chennai Super Kings            208
Rajasthan Royals               192
Kings XI Punjab                190
Delhi Daredevils               161
Sunrisers Hyderabad            152
Deccan Chargers                 75
Delhi Capitals                  63
Pune Warriors                   46
Gujarat Lions                   30
Punjab Kings                    28
Gujarat Titans                  16
Rising Pune Supergiant          16
Lucknow Super Giants            15
Kochi Tuskers Kerala            14
Rising Pune Supergiants         14
dtype: int64

# sort_values(series and dataframe) -> ascending -> na_position -> inplace -> multiple cols
     

x = pd.Series([12,14,1,56,89])
x
     
0    12
1    14
2     1
3    56
4    89
dtype: int64

x.sort_values(ascending=False)
     
4    89
3    56
1    14
0    12
2     1
dtype: int64

movies = pd.read_csv('movies.csv')
movies.head(4)
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)
3	Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)

movies.sort_values('title_x',ascending=False)
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
1623	Zubeidaa	tt0255713	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zubeidaa	Zubeidaa	Zubeidaa	0	2001	153	Biography|Drama|History	6.2	1384	The film begins with Riyaz (Rajat Kapoor) Zub...	Zubeidaa an aspiring Muslim actress marries ...	The Story of a Princess	Karisma Kapoor|Rekha|Manoj Bajpayee|Rajit Kapo...	3 wins & 13 nominations	19 January 2001 (India)
939	Zor Lagaa Ke...Haiya!	tt1479857	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zor_Lagaa_Ke...H...	Zor Lagaa Ke... Haiya!	Zor Lagaa Ke... Haiya!	0	2009	\N	Comedy|Drama|Family	6.4	46	A tree narrates the story of four Mumbai-based...	Children build a tree-house to spy on a beggar...	NaN	Meghan Jadhav|Mithun Chakraborty|Riya Sen|Seem...	NaN	NaN
756	Zokkomon	tt1605790	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zokkomon	Zokkomon	Zokkomon	0	2011	109	Action|Adventure	4.0	274	After the passing of his parents in an acciden...	An orphan is abused and abandoned believed to...	Betrayal. Friendship. Bravery.	Darsheel Safary|Anupam Kher|Manjari Fadnnis|Ti...	NaN	22 April 2011 (India)
670	Zindagi Tere Naam	tt2164702	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zindagi_Tere_Naam	Zindagi Tere Naam	Zindagi Tere Naam	0	2012	120	Romance	4.7	27	Mr. Singh an elderly gentleman relates to hi...	Mr. Singh an elderly gentleman relates to hi...	NaN	Mithun Chakraborty|Ranjeeta Kaur|Priyanka Meht...	1 win	16 March 2012 (India)
778	Zindagi Na Milegi Dobara	tt1562872	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zindagi_Na_Mileg...	Zindagi Na Milegi Dobara	Zindagi Na Milegi Dobara	0	2011	155	Comedy|Drama	8.1	60826	Three friends decide to turn their fantasy vac...	Three friends decide to turn their fantasy vac...	NaN	Hrithik Roshan|Farhan Akhtar|Abhay Deol|Katrin...	30 wins & 22 nominations	15 July 2011 (India)
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
1039	1971 (2007 film)	tt0983990	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/1971_(2007_film)	1971	1971	0	2007	160	Action|Drama|War	7.9	1121	Based on true facts the film revolves around ...	Based on true facts the film revolves around ...	Honor the heroes.......	Manoj Bajpayee|Ravi Kishan|Deepak Dobriyal|	1 win	9 March 2007 (India)
723	1920: The Evil Returns	tt2222550	https://upload.wikimedia.org/wikipedia/en/e/e7...	https://en.wikipedia.org/wiki/1920:_The_Evil_R...	1920: Evil Returns	1920: Evil Returns	0	2012	124	Drama|Horror|Romance	4.8	1587	This story revolves around a famous poet who m...	This story revolves around a famous poet who m...	Possession is back	Vicky Ahuja|Tia Bajpai|Irma Jämhammar|Sharad K...	NaN	2 November 2012 (India)
287	1920: London	tt5638500	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/1920_London	1920 London	1920 London	0	2016	120	Horror|Mystery	4.1	1373	Shivangi (Meera Chopra) lives in London with h...	After her husband is possessed by an evil spir...	Fear strikes again	Sharman Joshi|Meera Chopra|Vishal Karwal|Suren...	NaN	6 May 2016 (USA)
1021	1920 (film)	tt1301698	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/1920_(film)	1920	1920	0	2008	138	Horror|Mystery|Romance	6.4	2588	A devotee of Bhagwan Shri Hanuman Arjun Singh...	After forsaking his family and religion a hus...	A Love Made in Heaven...A Revenge Born in Hell...	Rajniesh Duggall|Adah Sharma|Anjori Alagh|Raj ...	NaN	12 September 2008 (India)
1498	16 December (film)	tt0313844	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/16_December_(film)	16-Dec	16-Dec	0	2002	158	Action|Thriller	6.9	1091	16 December 1971 was the day when India won t...	Indian intelligence agents race against time t...	NaN	Danny Denzongpa|Gulshan Grover|Milind Soman|Di...	2 nominations	22 March 2002 (India)
1629 rows × 18 columns



     

students = pd.DataFrame(
    {
        'name':['nitish','ankit','rupesh',np.nan,'mrityunjay',np.nan,'rishabh',np.nan,'aditya',np.nan],
        'college':['bit','iit','vit',np.nan,np.nan,'vlsi','ssit',np.nan,np.nan,'git'],
        'branch':['eee','it','cse',np.nan,'me','ce','civ','cse','bio',np.nan],
        'cgpa':[6.66,8.25,6.41,np.nan,5.6,9.0,7.4,10,7.4,np.nan],
        'package':[4,5,6,np.nan,6,7,8,9,np.nan,np.nan]

    }
)

students
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students.sort_values('name',na_position='first',ascending=False,inplace=True)
     

students
     
name	college	branch	cgpa	package
3	NaN	NaN	NaN	NaN	NaN
5	NaN	vlsi	ce	9.00	7.0
7	NaN	NaN	cse	10.00	9.0
9	NaN	git	NaN	NaN	NaN
2	rupesh	vit	cse	6.41	6.0
6	rishabh	ssit	civ	7.40	8.0
0	nitish	bit	eee	6.66	4.0
4	mrityunjay	NaN	me	5.60	6.0
1	ankit	iit	it	8.25	5.0
8	aditya	NaN	bio	7.40	NaN

movies.sort_values(['year_of_release','title_x'],ascending=[True,False])
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
1623	Zubeidaa	tt0255713	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Zubeidaa	Zubeidaa	Zubeidaa	0	2001	153	Biography|Drama|History	6.2	1384	The film begins with Riyaz (Rajat Kapoor) Zub...	Zubeidaa an aspiring Muslim actress marries ...	The Story of a Princess	Karisma Kapoor|Rekha|Manoj Bajpayee|Rajit Kapo...	3 wins & 13 nominations	19 January 2001 (India)
1625	Yeh Zindagi Ka Safar	tt0298607	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Zindagi_Ka_S...	Yeh Zindagi Ka Safar	Yeh Zindagi Ka Safar	0	2001	146	Drama	3.0	133	Hindi pop-star Sarina Devan lives a wealthy ...	A singer finds out she was adopted when the ed...	NaN	Ameesha Patel|Jimmy Sheirgill|Nafisa Ali|Gulsh...	NaN	16 November 2001 (India)
1622	Yeh Teraa Ghar Yeh Meraa Ghar	tt0298606	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Teraa_Ghar_Y...	Yeh Teraa Ghar Yeh Meraa Ghar	Yeh Teraa Ghar Yeh Meraa Ghar	0	2001	175	Comedy|Drama	5.7	704	In debt; Dayashankar Pandey is forced to go to...	In debt; Dayashankar Pandey is forced to go to...	NaN	Sunil Shetty|Mahima Chaudhry|Paresh Rawal|Saur...	1 nomination	12 October 2001 (India)
1620	Yeh Raaste Hain Pyaar Ke	tt0292740	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Raaste_Hain_...	Yeh Raaste Hain Pyaar Ke	Yeh Raaste Hain Pyaar Ke	0	2001	149	Drama|Romance	4.0	607	Two con artistes and car thieves Vicky (Ajay ...	Two con artistes and car thieves Vicky (Ajay ...	Love is a journey... not a destination	Ajay Devgn|Madhuri Dixit|Preity Zinta|Vikram G...	NaN	10 August 2001 (India)
1573	Yaadein (2001 film)	tt0248617	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yaadein_(2001_film)	Yaadein...	Yaadein...	0	2001	171	Drama|Musical|Romance	4.4	3034	Raj Singh Puri is best friends with L.K. Malho...	Raj Singh Puri is best friends with L.K. Malho...	memories to cherish...	Jackie Shroff|Hrithik Roshan|Kareena Kapoor|Am...	1 nomination	27 June 2001 (India)
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
37	Article 15 (film)	tt10324144	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Article_15_(film)	Article 15	Article 15	0	2019	130	Crime|Drama	8.3	13417	In the rural heartlands of India an upright p...	In the rural heartlands of India an upright p...	Farq Bahut Kar Liya| Ab Farq Laayenge.	Ayushmann Khurrana|Nassar|Manoj Pahwa|Kumud Mi...	1 win	28 June 2019 (USA)
46	Arjun Patiala	tt7881524	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Arjun_Patiala	Arjun Patiala	Arjun Patiala	0	2019	107	Action|Comedy	4.1	676	Arjun Patiala(Diljit Dosanjh)has recently been...	This spoof comedy narrates the story of a cop ...	NaN	Diljit Dosanjh|Kriti Sanon|Varun Sharma|Ronit ...	NaN	26 July 2019 (USA)
10	Amavas	tt8396186	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Amavas	Amavas	Amavas	0	2019	134	Horror|Thriller	2.8	235	Far away from the bustle of the city a young ...	The lives of a couple turn into a nightmare a...	NaN	Ali Asgar|Vivan Bhatena|Nargis Fakhri|Sachiin ...	NaN	8 February 2019 (India)
26	Albert Pinto Ko Gussa Kyun Aata Hai?	tt4355838	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Albert_Pinto_Ko_...	Albert Pinto Ko Gussa Kyun Aata Hai?	Albert Pinto Ko Gussa Kyun Aata Hai?	0	2019	100	Drama	4.8	56	Albert leaves his house one morning without te...	Albert Pinto goes missing one day and his girl...	NaN	Manav Kaul|Nandita Das|	NaN	12 April 2019 (India)
21	22 Yards	tt9496212	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/22_Yards	22 Yards	22 Yards	0	2019	126	Sport	5.3	124	A dramatic portrayal of a victorious tale of a...	A dramatic portrayal of a victorious tale of a...	NaN	Barun Sobti|Rajit Kapur|Panchhi Bora|Kartikey ...	NaN	15 March 2019 (India)
1629 rows × 18 columns



     

# rank(series)
batsman = pd.read_csv('batsman_runs_ipl.csv')
batsman.head()
     
batter	batsman_run
0	A Ashish Reddy	280
1	A Badoni	161
2	A Chandila	4
3	A Chopra	53
4	A Choudhary	25

batsman['batting_rank'] = batsman['batsman_run'].rank(ascending=False)
batsman.sort_values('batting_rank')
     
batter	batsman_run	batting_rank
569	V Kohli	6634	1.0
462	S Dhawan	6244	2.0
130	DA Warner	5883	3.0
430	RG Sharma	5881	4.0
493	SK Raina	5536	5.0
...	...	...	...
512	SS Cottrell	0	594.0
466	S Kaushik	0	594.0
203	IC Pandey	0	594.0
467	S Ladda	0	594.0
468	S Lamichhane	0	594.0
605 rows × 3 columns


# sort_index(series and dataframe)
     

marks = {
    'maths':67,
    'english':57,
    'science':89,
    'hindi':100
}

marks_series = pd.Series(marks)
marks_series
     
maths       67
english     57
science     89
hindi      100
dtype: int64

marks_series.sort_index(ascending=False)
     
science     89
maths       67
hindi      100
english     57
dtype: int64

movies.sort_index(ascending=False)
     
title_x	imdb_id	poster_path	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
1628	Humsafar	tt2403201	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Humsafar	Humsafar	Humsafar	0	2011	35	Drama|Romance	9.0	2968	Sara and Ashar are childhood friends who share...	Ashar and Khirad are forced to get married due...	NaN	Fawad Khan|	NaN	TV Series (2011–2012)
1627	Daaka	tt10833860	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Daaka	Daaka	Daaka	0	2019	136	Action	7.4	38	Shinda tries robbing a bank so he can be wealt...	Shinda tries robbing a bank so he can be wealt...	NaN	Gippy Grewal|Zareen Khan|	NaN	1 November 2019 (USA)
1626	Sabse Bada Sukh	tt0069204	NaN	https://en.wikipedia.org/wiki/Sabse_Bada_Sukh	Sabse Bada Sukh	Sabse Bada Sukh	0	2018	\N	Comedy|Drama	6.1	13	Village born Lalloo re-locates to Bombay and ...	Village born Lalloo re-locates to Bombay and ...	NaN	Vijay Arora|Asrani|Rajni Bala|Kumud Damle|Utpa...	NaN	NaN
1625	Yeh Zindagi Ka Safar	tt0298607	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Zindagi_Ka_S...	Yeh Zindagi Ka Safar	Yeh Zindagi Ka Safar	0	2001	146	Drama	3.0	133	Hindi pop-star Sarina Devan lives a wealthy ...	A singer finds out she was adopted when the ed...	NaN	Ameesha Patel|Jimmy Sheirgill|Nafisa Ali|Gulsh...	NaN	16 November 2001 (India)
1624	Tera Mera Saath Rahen	tt0301250	https://upload.wikimedia.org/wikipedia/en/2/2b...	https://en.wikipedia.org/wiki/Tera_Mera_Saath_...	Tera Mera Saath Rahen	Tera Mera Saath Rahen	0	2001	148	Drama	4.9	278	Raj Dixit lives with his younger brother Rahu...	A man is torn between his handicapped brother ...	NaN	Ajay Devgn|Sonali Bendre|Namrata Shirodkar|Pre...	NaN	7 November 2001 (India)
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
4	Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)
3	Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)
2	The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)
1	Battalion 609	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)
0	Uri: The Surgical Strike	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
1629 rows × 18 columns


# set_index(dataframe) -> inplace
batsman.set_index('batter',inplace=True)
     

batsman
     
batsman_run	batting_rank
batter		
A Ashish Reddy	280	166.5
A Badoni	161	226.0
A Chandila	4	535.0
A Chopra	53	329.0
A Choudhary	25	402.5
...	...	...
Yash Dayal	0	594.0
Yashpal Singh	47	343.0
Younis Khan	3	547.5
Yuvraj Singh	2754	27.0
Z Khan	117	256.0
605 rows × 2 columns


# reset_index(series + dataframe) -> drop parameter
batsman.reset_index(inplace=True)
     
batter	batsman_run	batting_rank
0	A Ashish Reddy	280	166.5
1	A Badoni	161	226.0
2	A Chandila	4	535.0
3	A Chopra	53	329.0
4	A Choudhary	25	402.5
...	...	...	...
600	Yash Dayal	0	594.0
601	Yashpal Singh	47	343.0
602	Younis Khan	3	547.5
603	Yuvraj Singh	2754	27.0
604	Z Khan	117	256.0
605 rows × 3 columns


batsman
     
batsman_run	batting_rank
batter		
A Ashish Reddy	280	166.5
A Badoni	161	226.0
A Chandila	4	535.0
A Chopra	53	329.0
A Choudhary	25	402.5
...	...	...
Yash Dayal	0	594.0
Yashpal Singh	47	343.0
Younis Khan	3	547.5
Yuvraj Singh	2754	27.0
Z Khan	117	256.0
605 rows × 2 columns


# how to replace existing index without loosing
batsman.reset_index().set_index('batting_rank')
     
batter	batsman_run
batting_rank		
166.5	A Ashish Reddy	280
226.0	A Badoni	161
535.0	A Chandila	4
329.0	A Chopra	53
402.5	A Choudhary	25
...	...	...
594.0	Yash Dayal	0
343.0	Yashpal Singh	47
547.5	Younis Khan	3
27.0	Yuvraj Singh	2754
256.0	Z Khan	117
605 rows × 2 columns


# series to dataframe using reset_index
marks_series.reset_index()
     
index	0
0	maths	67
1	english	57
2	science	89
3	hindi	100

# rename(dataframe) -> index
     

movies.set_index('title_x',inplace=True)
     

movies.rename(columns={'imdb_id':'imdb','poster_path':'link'},inplace=True)
     

movies.rename(index={'Uri: The Surgical Strike':'Uri','Battalion 609':'Battalion'})
     
imdb	link	wiki_link	title_y	original_title	is_adult	year_of_release	runtime	genres	imdb_rating	imdb_votes	story	summary	tagline	actors	wins_nominations	release_date
title_x																	
Uri	tt8291224	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Uri:_The_Surgica...	Uri: The Surgical Strike	Uri: The Surgical Strike	0	2019	138	Action|Drama|War	8.4	35112	Divided over five chapters the film chronicle...	Indian army special forces execute a covert op...	NaN	Vicky Kaushal|Paresh Rawal|Mohit Raina|Yami Ga...	4 wins	11 January 2019 (USA)
Battalion	tt9472208	NaN	https://en.wikipedia.org/wiki/Battalion_609	Battalion 609	Battalion 609	0	2019	131	War	4.1	73	The story revolves around a cricket match betw...	The story of Battalion 609 revolves around a c...	NaN	Vicky Ahuja|Shoaib Ibrahim|Shrikant Kamat|Elen...	NaN	11 January 2019 (India)
The Accidental Prime Minister (film)	tt6986710	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/The_Accidental_P...	The Accidental Prime Minister	The Accidental Prime Minister	0	2019	112	Biography|Drama	6.1	5549	Based on the memoir by Indian policy analyst S...	Explores Manmohan Singh's tenure as the Prime ...	NaN	Anupam Kher|Akshaye Khanna|Aahana Kumra|Atul S...	NaN	11 January 2019 (USA)
Why Cheat India	tt8108208	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Why_Cheat_India	Why Cheat India	Why Cheat India	0	2019	121	Crime|Drama	6.0	1891	The movie focuses on existing malpractices in ...	The movie focuses on existing malpractices in ...	NaN	Emraan Hashmi|Shreya Dhanwanthary|Snighdadeep ...	NaN	18 January 2019 (USA)
Evening Shadows	tt6028796	NaN	https://en.wikipedia.org/wiki/Evening_Shadows	Evening Shadows	Evening Shadows	0	2018	102	Drama	7.3	280	While gay rights and marriage equality has bee...	Under the 'Evening Shadows' truth often plays...	NaN	Mona Ambegaonkar|Ananth Narayan Mahadevan|Deva...	17 wins & 1 nomination	11 January 2019 (India)
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
Tera Mera Saath Rahen	tt0301250	https://upload.wikimedia.org/wikipedia/en/2/2b...	https://en.wikipedia.org/wiki/Tera_Mera_Saath_...	Tera Mera Saath Rahen	Tera Mera Saath Rahen	0	2001	148	Drama	4.9	278	Raj Dixit lives with his younger brother Rahu...	A man is torn between his handicapped brother ...	NaN	Ajay Devgn|Sonali Bendre|Namrata Shirodkar|Pre...	NaN	7 November 2001 (India)
Yeh Zindagi Ka Safar	tt0298607	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Yeh_Zindagi_Ka_S...	Yeh Zindagi Ka Safar	Yeh Zindagi Ka Safar	0	2001	146	Drama	3.0	133	Hindi pop-star Sarina Devan lives a wealthy ...	A singer finds out she was adopted when the ed...	NaN	Ameesha Patel|Jimmy Sheirgill|Nafisa Ali|Gulsh...	NaN	16 November 2001 (India)
Sabse Bada Sukh	tt0069204	NaN	https://en.wikipedia.org/wiki/Sabse_Bada_Sukh	Sabse Bada Sukh	Sabse Bada Sukh	0	2018	\N	Comedy|Drama	6.1	13	Village born Lalloo re-locates to Bombay and ...	Village born Lalloo re-locates to Bombay and ...	NaN	Vijay Arora|Asrani|Rajni Bala|Kumud Damle|Utpa...	NaN	NaN
Daaka	tt10833860	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Daaka	Daaka	Daaka	0	2019	136	Action	7.4	38	Shinda tries robbing a bank so he can be wealt...	Shinda tries robbing a bank so he can be wealt...	NaN	Gippy Grewal|Zareen Khan|	NaN	1 November 2019 (USA)
Humsafar	tt2403201	https://upload.wikimedia.org/wikipedia/en/thum...	https://en.wikipedia.org/wiki/Humsafar	Humsafar	Humsafar	0	2011	35	Drama|Romance	9.0	2968	Sara and Ashar are childhood friends who share...	Ashar and Khirad are forced to get married due...	NaN	Fawad Khan|	NaN	TV Series (2011–2012)
1629 rows × 17 columns


# unique(series)
temp = pd.Series([1,1,2,2,3,3,4,4,5,5,np.nan,np.nan])
print(temp)
     
0     1.0
1     1.0
2     2.0
3     2.0
4     3.0
5     3.0
6     4.0
7     4.0
8     5.0
9     5.0
10    NaN
11    NaN
dtype: float64

len(temp.unique())
     
6

temp.nunique()
     
5

len(ipl['Season'].unique())
     
15

# nunique(series + dataframe) -> does not count nan -> dropna parameter
ipl['Season'].nunique()
     
15

# isnull(series + dataframe)
students['name'][students['name'].isnull()]
     
3    NaN
5    NaN
7    NaN
9    NaN
Name: name, dtype: object

# notnull(series + dataframe)
students['name'][students['name'].notnull()]
     
0        nitish
1         ankit
2        rupesh
4    mrityunjay
6       rishabh
8        aditya
Name: name, dtype: object

# hasnans(series)
students['name'].hasnans
     
True

students
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students.isnull()
     
name	college	branch	cgpa	package
0	False	False	False	False	False
1	False	False	False	False	False
2	False	False	False	False	False
3	True	True	True	True	True
4	False	True	False	False	False
5	True	False	False	False	False
6	False	False	False	False	False
7	True	True	False	False	False
8	False	True	False	False	True
9	True	False	True	True	True

students.notnull()
     
name	college	branch	cgpa	package
0	True	True	True	True	True
1	True	True	True	True	True
2	True	True	True	True	True
3	False	False	False	False	False
4	True	False	True	True	True
5	False	True	True	True	True
6	True	True	True	True	True
7	False	False	True	True	True
8	True	False	True	True	False
9	False	True	False	False	False

# dropna(series + dataframe) -> how parameter -> works like or
students['name'].dropna()
     
0        nitish
1         ankit
2        rupesh
4    mrityunjay
6       rishabh
8        aditya
Name: name, dtype: object

students
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students.dropna(how='any')
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
6	rishabh	ssit	civ	7.40	8.0

students.dropna(how='all')
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students.dropna(subset=['name'])
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
4	mrityunjay	NaN	me	5.60	6.0
6	rishabh	ssit	civ	7.40	8.0
8	aditya	NaN	bio	7.40	NaN

students.dropna(subset=['name','college'])
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
6	rishabh	ssit	civ	7.40	8.0

students.dropna(inplace=True)
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

# fillna(series + dataframe)
students['name'].fillna('unknown')
     
0        nitish
1         ankit
2        rupesh
3       unknown
4    mrityunjay
5       unknown
6       rishabh
7       unknown
8        aditya
9       unknown
Name: name, dtype: object

students
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students['package'].fillna(students['package'].mean())
     
0    4.000000
1    5.000000
2    6.000000
3    6.428571
4    6.000000
5    7.000000
6    8.000000
7    9.000000
8    6.428571
9    6.428571
Name: package, dtype: float64

students['name'].fillna(method='bfill')
     
0        nitish
1         ankit
2        rupesh
3    mrityunjay
4    mrityunjay
5       rishabh
6       rishabh
7        aditya
8        aditya
9           NaN
Name: name, dtype: object

# drop_duplicates(series + dataframe) -> works like and -> duplicated()
     

temp = pd.Series([1,1,1,2,3,3,4,4])
temp.drop_duplicates()
     
0    1
3    2
4    3
6    4
dtype: int64

marks.drop_duplicates(keep='last')
     
iq	marks	package
0	100	80	10
1	90	70	7
2	120	100	14
4	80	70	14

# find the last match played by virat kohli in Delhi
ipl['all_players'] = ipl['Team1Players'] + ipl['Team2Players']
ipl.head()
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	...	WinningTeam	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2	all_players
0	1312200	Ahmedabad	2022-05-29	2022	Final	Rajasthan Royals	Gujarat Titans	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	bat	...	Gujarat Titans	Wickets	7.0	NaN	HH Pandya	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pan...	CB Gaffaney	Nitin Menon	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...
1	1312199	Ahmedabad	2022-05-27	2022	Qualifier 2	Royal Challengers Bangalore	Rajasthan Royals	Narendra Modi Stadium, Ahmedabad	Rajasthan Royals	field	...	Rajasthan Royals	Wickets	7.0	NaN	JC Buttler	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	CB Gaffaney	Nitin Menon	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...
2	1312198	Kolkata	2022-05-25	2022	Eliminator	Royal Challengers Bangalore	Lucknow Super Giants	Eden Gardens, Kolkata	Lucknow Super Giants	field	...	Royal Challengers Bangalore	Runs	14.0	NaN	RM Patidar	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...	['Q de Kock', 'KL Rahul', 'M Vohra', 'DJ Hooda...	J Madanagopal	MA Gough	['V Kohli', 'F du Plessis', 'RM Patidar', 'GJ ...
3	1312197	Kolkata	2022-05-24	2022	Qualifier 1	Rajasthan Royals	Gujarat Titans	Eden Gardens, Kolkata	Gujarat Titans	field	...	Gujarat Titans	Wickets	7.0	NaN	DA Miller	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...	['WP Saha', 'Shubman Gill', 'MS Wade', 'HH Pan...	BNJ Oxenford	VK Sharma	['YBK Jaiswal', 'JC Buttler', 'SV Samson', 'D ...
4	1304116	Mumbai	2022-05-22	2022	70	Sunrisers Hyderabad	Punjab Kings	Wankhede Stadium, Mumbai	Sunrisers Hyderabad	bat	...	Punjab Kings	Wickets	5.0	NaN	Harpreet Brar	['PK Garg', 'Abhishek Sharma', 'RA Tripathi', ...	['JM Bairstow', 'S Dhawan', 'M Shahrukh Khan',...	AK Chaudhary	NA Patwardhan	['PK Garg', 'Abhishek Sharma', 'RA Tripathi', ...
5 rows × 21 columns


def did_kohli_play(players_list):
  return 'V Kohli' in players_list
     

ipl['did_kohli_play'] = ipl['all_players'].apply(did_kohli_play)
ipl[(ipl['City'] == 'Delhi') & (ipl['did_kohli_play'] == True)].drop_duplicates(subset=['City','did_kohli_play'],keep='first')
     
ID	City	Date	Season	MatchNumber	Team1	Team2	Venue	TossWinner	TossDecision	...	WonBy	Margin	method	Player_of_Match	Team1Players	Team2Players	Umpire1	Umpire2	all_players	did_kohli_play
208	1178421	Delhi	2019-04-28	2019	46	Delhi Capitals	Royal Challengers Bangalore	Arun Jaitley Stadium	Delhi Capitals	bat	...	Runs	16.0	NaN	S Dhawan	['PP Shaw', 'S Dhawan', 'SS Iyer', 'RR Pant', ...	['PA Patel', 'V Kohli', 'AB de Villiers', 'S D...	BNJ Oxenford	KN Ananthapadmanabhan	['PP Shaw', 'S Dhawan', 'SS Iyer', 'RR Pant', ...	True
1 rows × 22 columns


students.drop_duplicates()
     

# drop(series + dataframe)
temp = pd.Series([10,2,3,16,45,78,10])
temp
     
0    10
1     2
2     3
3    16
4    45
5    78
6    10
dtype: int64

temp.drop(index=[0,6])
     
1     2
2     3
3    16
4    45
5    78
dtype: int64

students
     
name	college	branch	cgpa	package
0	nitish	bit	eee	6.66	4.0
1	ankit	iit	it	8.25	5.0
2	rupesh	vit	cse	6.41	6.0
3	NaN	NaN	NaN	NaN	NaN
4	mrityunjay	NaN	me	5.60	6.0
5	NaN	vlsi	ce	9.00	7.0
6	rishabh	ssit	civ	7.40	8.0
7	NaN	NaN	cse	10.00	9.0
8	aditya	NaN	bio	7.40	NaN
9	NaN	git	NaN	NaN	NaN

students.drop(columns=['branch','cgpa'],inplace=True)
     
name	college	package
0	nitish	bit	4.0
1	ankit	iit	5.0
2	rupesh	vit	6.0
3	NaN	NaN	NaN
4	mrityunjay	NaN	6.0
5	NaN	vlsi	7.0
6	rishabh	ssit	8.0
7	NaN	NaN	9.0
8	aditya	NaN	NaN
9	NaN	git	NaN

students.set_index('name').drop(index=['nitish','aditya'])
     
college	branch	cgpa	package
name				
ankit	iit	it	8.25	5.0
rupesh	vit	cse	6.41	6.0
NaN	NaN	NaN	NaN	NaN
mrityunjay	NaN	me	5.60	6.0
NaN	vlsi	ce	9.00	7.0
rishabh	ssit	civ	7.40	8.0
NaN	NaN	cse	10.00	9.0
NaN	git	NaN	NaN	NaN

# apply(series + dataframe)
temp = pd.Series([10,20,30,40,50])

temp
     
0    10
1    20
2    30
3    40
4    50
dtype: int64

def sigmoid(value):
  return 1/1+np.exp(-value)
     

temp.apply(sigmoid)
     
0    1.000045
1    1.000000
2    1.000000
3    1.000000
4    1.000000
dtype: float64

points_df = pd.DataFrame(
    {
        '1st point':[(3,4),(-6,5),(0,0),(-10,1),(4,5)],
        '2nd point':[(-3,4),(0,0),(2,2),(10,10),(1,1)]
    }
)

points_df
     
1st point	2nd point
0	(3, 4)	(-3, 4)
1	(-6, 5)	(0, 0)
2	(0, 0)	(2, 2)
3	(-10, 1)	(10, 10)
4	(4, 5)	(1, 1)

def euclidean(row):
  pt_A = row['1st point']
  pt_B = row['2nd point']

  return ((pt_A[0] - pt_B[0])**2 + (pt_A[1] - pt_B[1])**2)**0.5
     

points_df['distance'] = points_df.apply(euclidean,axis=1)
points_df
     
1st point	2nd point	distance
0	(3, 4)	(-3, 4)	6.000000
1	(-6, 5)	(0, 0)	7.810250
2	(0, 0)	(2, 2)	2.828427
3	(-10, 1)	(10, 10)	21.931712
4	(4, 5)	(1, 1)	5.000000

# isin(series)
     


     

# corr
     


     


     

# nlargest and nsmallest(series and dataframe)
     

# insert(dataframe)

     


     

# copy(series + dataframe)
     


     
###################################################################################



import pandas as pd
import numpy as np
     

movies = pd.read_csv('/content/imdb-top-1000.csv')
     

movies.head()
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
0	The Shawshank Redemption	1994	142	Drama	9.3	Frank Darabont	Tim Robbins	2343110	28341469.0	80.0
1	The Godfather	1972	175	Crime	9.2	Francis Ford Coppola	Marlon Brando	1620367	134966411.0	100.0
2	The Dark Knight	2008	152	Action	9.0	Christopher Nolan	Christian Bale	2303232	534858444.0	84.0
3	The Godfather: Part II	1974	202	Crime	9.0	Francis Ford Coppola	Al Pacino	1129952	57300000.0	90.0
4	12 Angry Men	1957	96	Crime	9.0	Sidney Lumet	Henry Fonda	689845	4360000.0	96.0

genres = movies.groupby('Genre')
     

# Applying builtin aggregation fuctions on groupby objects
genres.std()
     
Runtime	IMDB_Rating	No_of_Votes	Gross	Metascore
Genre					
Action	28.500706	0.304258	432946.814748	2.256724e+08	12.421252
Adventure	33.317320	0.229781	301188.347642	1.697543e+08	12.345393
Animation	14.530471	0.253221	262173.231571	2.091840e+08	8.813646
Biography	25.514466	0.267140	271284.191372	1.363251e+08	11.028187
Comedy	22.946213	0.228771	188653.570564	1.946513e+08	11.829160
Crime	27.689231	0.335477	373999.730656	1.571191e+08	13.099102
Drama	27.740490	0.267229	305554.162841	2.201164e+08	12.744687
Family	10.606602	0.000000	137008.302816	3.048412e+08	16.970563
Fantasy	12.727922	0.141421	22179.111299	7.606861e+07	NaN
Film-Noir	4.000000	0.152753	54649.083277	7.048472e+07	1.527525
Horror	13.604812	0.311302	234883.508691	9.965017e+07	15.362291
Mystery	14.475423	0.310791	404621.915297	1.567524e+08	18.604435
Thriller	NaN	NaN	NaN	NaN	NaN
Western	17.153717	0.420317	263489.554280	1.230626e+07	9.032349

# find the top 3 genres by total earning
movies.groupby('Genre').sum()['Gross'].sort_values(ascending=False).head(3)
     
Genre
Drama     3.540997e+10
Action    3.263226e+10
Comedy    1.566387e+10
Name: Gross, dtype: float64

movies.groupby('Genre')['Gross'].sum().sort_values(ascending=False).head(3)
     
Genre
Drama     3.540997e+10
Action    3.263226e+10
Comedy    1.566387e+10
Name: Gross, dtype: float64

# find the genre with highest avg IMDB rating
movies.groupby('Genre')['IMDB_Rating'].mean().sort_values(ascending=False).head(1)
     
Genre
Western    8.35
Name: IMDB_Rating, dtype: float64

# find director with most popularity
movies.groupby('Director')['No_of_Votes'].sum().sort_values(ascending=False).head(1)
     
Director
Christopher Nolan    11578345
Name: No_of_Votes, dtype: int64

# find the highest rated movie of each genre
# movies.groupby('Genre')['IMDB_Rating'].max()
     
Genre
Action       9.0
Adventure    8.6
Animation    8.6
Biography    8.9
Comedy       8.6
Crime        9.2
Drama        9.3
Family       7.8
Fantasy      8.1
Film-Noir    8.1
Horror       8.5
Mystery      8.4
Thriller     7.8
Western      8.8
Name: IMDB_Rating, dtype: float64

# find number of movies done by each actor
# movies['Star1'].value_counts()

movies.groupby('Star1')['Series_Title'].count().sort_values(ascending=False)
     
Star1
Tom Hanks             12
Robert De Niro        11
Clint Eastwood        10
Al Pacino             10
Leonardo DiCaprio      9
                      ..
Glen Hansard           1
Giuseppe Battiston     1
Giulietta Masina       1
Gerardo Taracena       1
Ömer Faruk Sorak       1
Name: Series_Title, Length: 660, dtype: int64

# GroupBy Attributes and Methods
# find total number of groups -> len
# find items in each group -> size
# first()/last() -> nth item
# get_group -> vs filtering
# groups
# describe
# sample
# nunique
     

len(movies.groupby('Genre'))
     
14

movies['Genre'].nunique()
     
14

movies.groupby('Genre').size()
     
Genre
Action       172
Adventure     72
Animation     82
Biography     88
Comedy       155
Crime        107
Drama        289
Family         2
Fantasy        2
Film-Noir      3
Horror        11
Mystery       12
Thriller       1
Western        4
dtype: int64

genres = movies.groupby('Genre')
# genres.first()
# genres.last()
genres.nth(6)
     
Series_Title	Released_Year	Runtime	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
Genre									
Action	Star Wars: Episode V - The Empire Strikes Back	1980	124	8.7	Irvin Kershner	Mark Hamill	1159315	290475067.0	82.0
Adventure	North by Northwest	1959	136	8.3	Alfred Hitchcock	Cary Grant	299198	13275000.0	98.0
Animation	WALL·E	2008	98	8.4	Andrew Stanton	Ben Burtt	999790	223808164.0	95.0
Biography	Braveheart	1995	178	8.3	Mel Gibson	Mel Gibson	959181	75600000.0	68.0
Comedy	The Great Dictator	1940	125	8.4	Charles Chaplin	Charles Chaplin	203150	288475.0	NaN
Crime	Se7en	1995	127	8.6	David Fincher	Morgan Freeman	1445096	100125643.0	65.0
Drama	It's a Wonderful Life	1946	130	8.6	Frank Capra	James Stewart	405801	82385199.0	89.0
Horror	Get Out	2017	104	7.7	Jordan Peele	Daniel Kaluuya	492851	176040665.0	85.0
Mystery	Sleuth	1972	138	8.0	Joseph L. Mankiewicz	Laurence Olivier	44748	4081254.0	NaN

movies['Genre'].value_counts()
     
Drama        289
Action       172
Comedy       155
Crime        107
Biography     88
Animation     82
Adventure     72
Mystery       12
Horror        11
Western        4
Film-Noir      3
Fantasy        2
Family         2
Thriller       1
Name: Genre, dtype: int64

genres.get_group('Fantasy')

movies[movies['Genre'] == 'Fantasy']
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
321	Das Cabinet des Dr. Caligari	1920	76	Fantasy	8.1	Robert Wiene	Werner Krauss	57428	337574718.0	NaN
568	Nosferatu	1922	94	Fantasy	7.9	F.W. Murnau	Max Schreck	88794	445151978.0	NaN

genres.groups
     
{'Action': [2, 5, 8, 10, 13, 14, 16, 29, 30, 31, 39, 42, 44, 55, 57, 59, 60, 63, 68, 72, 106, 109, 129, 130, 134, 140, 142, 144, 152, 155, 160, 161, 166, 168, 171, 172, 177, 181, 194, 201, 202, 216, 217, 223, 224, 236, 241, 262, 275, 294, 308, 320, 325, 326, 331, 337, 339, 340, 343, 345, 348, 351, 353, 356, 357, 362, 368, 369, 375, 376, 390, 410, 431, 436, 473, 477, 479, 482, 488, 493, 496, 502, 507, 511, 532, 535, 540, 543, 564, 569, 570, 573, 577, 582, 583, 602, 605, 608, 615, 623, ...], 'Adventure': [21, 47, 93, 110, 114, 116, 118, 137, 178, 179, 191, 193, 209, 226, 231, 247, 267, 273, 281, 300, 301, 304, 306, 323, 329, 361, 366, 377, 402, 406, 415, 426, 458, 470, 497, 498, 506, 513, 514, 537, 549, 552, 553, 566, 576, 604, 609, 618, 638, 647, 675, 681, 686, 692, 711, 713, 739, 755, 781, 797, 798, 851, 873, 884, 912, 919, 947, 957, 964, 966, 984, 991], 'Animation': [23, 43, 46, 56, 58, 61, 66, 70, 101, 135, 146, 151, 158, 170, 197, 205, 211, 213, 219, 229, 230, 242, 245, 246, 270, 330, 332, 358, 367, 378, 386, 389, 394, 395, 399, 401, 405, 409, 469, 499, 510, 516, 518, 522, 578, 586, 592, 595, 596, 599, 633, 640, 643, 651, 665, 672, 694, 728, 740, 741, 744, 756, 758, 761, 771, 783, 796, 799, 822, 828, 843, 875, 891, 892, 902, 906, 920, 956, 971, 976, 986, 992], 'Biography': [7, 15, 18, 35, 38, 54, 102, 107, 131, 139, 147, 157, 159, 173, 176, 212, 215, 218, 228, 235, 243, 263, 276, 282, 290, 298, 317, 328, 338, 342, 346, 359, 360, 365, 372, 373, 385, 411, 416, 418, 424, 429, 484, 525, 536, 542, 545, 575, 579, 587, 600, 606, 614, 622, 632, 635, 644, 649, 650, 657, 671, 673, 684, 729, 748, 753, 757, 759, 766, 770, 779, 809, 810, 815, 820, 831, 849, 858, 877, 882, 897, 910, 915, 923, 940, 949, 952, 987], 'Comedy': [19, 26, 51, 52, 64, 78, 83, 95, 96, 112, 117, 120, 127, 128, 132, 153, 169, 183, 192, 204, 207, 208, 214, 221, 233, 238, 240, 250, 251, 252, 256, 261, 266, 277, 284, 311, 313, 316, 318, 322, 327, 374, 379, 381, 392, 396, 403, 413, 414, 417, 427, 435, 445, 446, 449, 455, 459, 460, 463, 464, 466, 471, 472, 475, 481, 490, 494, 500, 503, 509, 526, 528, 530, 531, 533, 538, 539, 541, 547, 557, 558, 562, 563, 565, 574, 591, 593, 594, 598, 613, 626, 630, 660, 662, 667, 679, 680, 683, 687, 701, ...], 'Crime': [1, 3, 4, 6, 22, 25, 27, 28, 33, 37, 41, 71, 77, 79, 86, 87, 103, 108, 111, 113, 123, 125, 133, 136, 162, 163, 164, 165, 180, 186, 187, 189, 198, 222, 232, 239, 255, 257, 287, 288, 299, 305, 335, 363, 364, 380, 384, 397, 437, 438, 441, 442, 444, 450, 451, 465, 474, 480, 485, 487, 505, 512, 519, 520, 523, 527, 546, 556, 560, 584, 597, 603, 607, 611, 621, 639, 653, 664, 669, 676, 695, 708, 723, 762, 763, 767, 775, 791, 795, 802, 811, 823, 827, 833, 885, 895, 921, 922, 926, 938, ...], 'Drama': [0, 9, 11, 17, 20, 24, 32, 34, 36, 40, 45, 50, 53, 62, 65, 67, 73, 74, 76, 80, 82, 84, 85, 88, 89, 90, 91, 92, 94, 97, 98, 99, 100, 104, 105, 121, 122, 124, 126, 138, 141, 143, 148, 149, 150, 154, 156, 167, 174, 175, 182, 184, 185, 188, 190, 195, 196, 199, 200, 203, 206, 210, 225, 227, 234, 237, 244, 248, 249, 253, 254, 258, 259, 260, 264, 265, 268, 269, 272, 274, 278, 279, 280, 283, 285, 286, 289, 291, 292, 293, 295, 296, 297, 302, 303, 307, 310, 312, 314, 315, ...], 'Family': [688, 698], 'Fantasy': [321, 568], 'Film-Noir': [309, 456, 712], 'Horror': [49, 75, 271, 419, 544, 707, 724, 844, 876, 932, 948], 'Mystery': [69, 81, 119, 145, 220, 393, 420, 714, 829, 899, 959, 961], 'Thriller': [700], 'Western': [12, 48, 115, 691]}

genres.describe()
     
Runtime	IMDB_Rating	...	Gross	Metascore
count	mean	std	min	25%	50%	75%	max	count	mean	...	75%	max	count	mean	std	min	25%	50%	75%	max
Genre																					
Action	172.0	129.046512	28.500706	45.0	110.75	127.5	143.25	321.0	172.0	7.949419	...	2.674437e+08	936662225.0	143.0	73.419580	12.421252	33.0	65.00	74.0	82.00	98.0
Adventure	72.0	134.111111	33.317320	88.0	109.00	127.0	149.00	228.0	72.0	7.937500	...	1.998070e+08	874211619.0	64.0	78.437500	12.345393	41.0	69.75	80.5	87.25	100.0
Animation	82.0	99.585366	14.530471	71.0	90.00	99.5	106.75	137.0	82.0	7.930488	...	2.520612e+08	873839108.0	75.0	81.093333	8.813646	61.0	75.00	82.0	87.50	96.0
Biography	88.0	136.022727	25.514466	93.0	120.00	129.0	146.25	209.0	88.0	7.938636	...	9.829924e+07	753585104.0	79.0	76.240506	11.028187	48.0	70.50	76.0	84.50	97.0
Comedy	155.0	112.129032	22.946213	68.0	96.00	106.0	124.50	188.0	155.0	7.901290	...	8.107809e+07	886752933.0	125.0	78.720000	11.829160	45.0	72.00	79.0	88.00	99.0
Crime	107.0	126.392523	27.689231	80.0	106.50	122.0	141.50	229.0	107.0	8.016822	...	7.102163e+07	790482117.0	87.0	77.080460	13.099102	47.0	69.50	77.0	87.00	100.0
Drama	289.0	124.737024	27.740490	64.0	105.00	121.0	137.00	242.0	289.0	7.957439	...	1.164461e+08	924558264.0	241.0	79.701245	12.744687	28.0	72.00	82.0	89.00	100.0
Family	2.0	107.500000	10.606602	100.0	103.75	107.5	111.25	115.0	2.0	7.800000	...	3.273329e+08	435110554.0	2.0	79.000000	16.970563	67.0	73.00	79.0	85.00	91.0
Fantasy	2.0	85.000000	12.727922	76.0	80.50	85.0	89.50	94.0	2.0	8.000000	...	4.182577e+08	445151978.0	0.0	NaN	NaN	NaN	NaN	NaN	NaN	NaN
Film-Noir	3.0	104.000000	4.000000	100.0	102.00	104.0	106.00	108.0	3.0	7.966667	...	6.273068e+07	123353292.0	3.0	95.666667	1.527525	94.0	95.00	96.0	96.50	97.0
Horror	11.0	102.090909	13.604812	71.0	98.00	103.0	109.00	122.0	11.0	7.909091	...	1.362817e+08	298791505.0	11.0	80.000000	15.362291	46.0	77.50	87.0	88.50	97.0
Mystery	12.0	119.083333	14.475423	96.0	110.75	117.5	130.25	138.0	12.0	7.975000	...	1.310949e+08	474203697.0	8.0	79.125000	18.604435	52.0	65.25	77.0	98.50	100.0
Thriller	1.0	108.000000	NaN	108.0	108.00	108.0	108.00	108.0	1.0	7.800000	...	1.755074e+07	17550741.0	1.0	81.000000	NaN	81.0	81.00	81.0	81.00	81.0
Western	4.0	148.250000	17.153717	132.0	134.25	148.0	162.00	165.0	4.0	8.350000	...	1.920000e+07	31800000.0	4.0	78.250000	9.032349	69.0	72.75	77.0	82.50	90.0
14 rows × 40 columns


genres.sample(2,replace=True)
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
944	Batoru rowaiaru	2000	114	Action	7.6	Kinji Fukasaku	Tatsuya Fujiwara	169091	195856489.0	81.0
625	Apocalypto	2006	139	Action	7.8	Mel Gibson	Gerardo Taracena	291018	50866635.0	68.0
991	Kelly's Heroes	1970	144	Adventure	7.6	Brian G. Hutton	Clint Eastwood	45338	1378435.0	50.0
300	Ben-Hur	1959	212	Adventure	8.1	William Wyler	Charlton Heston	219466	74700000.0	90.0
891	Incredibles 2	2018	118	Animation	7.6	Brad Bird	Craig T. Nelson	250057	608581744.0	80.0
389	The Iron Giant	1999	86	Animation	8.0	Brad Bird	Eli Marienthal	172083	23159305.0	85.0
536	All the President's Men	1976	138	Biography	7.9	Alan J. Pakula	Dustin Hoffman	103031	70600000.0	84.0
635	Walk the Line	2005	136	Biography	7.8	James Mangold	Joaquin Phoenix	234207	119519402.0	72.0
826	Barton Fink	1991	116	Comedy	7.7	Joel Coen	Ethan Coen	113240	6153939.0	69.0
732	Me and Earl and the Dying Girl	2015	105	Comedy	7.7	Alfonso Gomez-Rejon	Thomas Mann	123210	6743776.0	74.0
438	Touch of Evil	1958	95	Crime	8.0	Orson Welles	Charlton Heston	98431	2237659.0	99.0
222	Prisoners	2013	153	Crime	8.1	Denis Villeneuve	Hugh Jackman	601149	61002302.0	70.0
555	High Noon	1952	85	Drama	7.9	Fred Zinnemann	Gary Cooper	97222	9450000.0	89.0
314	Gone with the Wind	1939	238	Drama	8.1	Victor Fleming	George Cukor	290074	198676459.0	97.0
698	Willy Wonka & the Chocolate Factory	1971	100	Family	7.8	Mel Stuart	Gene Wilder	178731	4000000.0	67.0
698	Willy Wonka & the Chocolate Factory	1971	100	Family	7.8	Mel Stuart	Gene Wilder	178731	4000000.0	67.0
321	Das Cabinet des Dr. Caligari	1920	76	Fantasy	8.1	Robert Wiene	Werner Krauss	57428	337574718.0	NaN
321	Das Cabinet des Dr. Caligari	1920	76	Fantasy	8.1	Robert Wiene	Werner Krauss	57428	337574718.0	NaN
456	The Maltese Falcon	1941	100	Film-Noir	8.0	John Huston	Humphrey Bogart	148928	2108060.0	96.0
456	The Maltese Falcon	1941	100	Film-Noir	8.0	John Huston	Humphrey Bogart	148928	2108060.0	96.0
544	Night of the Living Dead	1968	96	Horror	7.9	George A. Romero	Duane Jones	116557	89029.0	89.0
707	The Innocents	1961	100	Horror	7.8	Jack Clayton	Deborah Kerr	27007	2616000.0	88.0
393	Twelve Monkeys	1995	129	Mystery	8.0	Terry Gilliam	Bruce Willis	578443	57141459.0	74.0
829	Spoorloos	1988	107	Mystery	7.7	George Sluizer	Bernard-Pierre Donnadieu	33982	367916835.0	NaN
700	Wait Until Dark	1967	108	Thriller	7.8	Terence Young	Audrey Hepburn	27733	17550741.0	81.0
700	Wait Until Dark	1967	108	Thriller	7.8	Terence Young	Audrey Hepburn	27733	17550741.0	81.0
12	Il buono, il brutto, il cattivo	1966	161	Western	8.8	Sergio Leone	Clint Eastwood	688390	6100000.0	90.0
115	Per qualche dollaro in più	1965	132	Western	8.3	Sergio Leone	Clint Eastwood	232772	15000000.0	74.0

genres.nunique()
     
Series_Title	Released_Year	Runtime	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
Genre									
Action	172	61	78	15	123	121	172	172	50
Adventure	72	49	58	10	59	59	72	72	33
Animation	82	35	41	11	51	77	82	82	29
Biography	88	44	56	13	76	72	88	88	40
Comedy	155	72	70	11	113	133	155	155	44
Crime	106	56	65	14	86	85	107	107	39
Drama	289	83	95	14	211	250	288	287	52
Family	2	2	2	1	2	2	2	2	2
Fantasy	2	2	2	2	2	2	2	2	0
Film-Noir	3	3	3	3	3	3	3	3	3
Horror	11	11	10	8	10	11	11	11	9
Mystery	12	11	10	8	10	11	12	12	7
Thriller	1	1	1	1	1	1	1	1	1
Western	4	4	4	4	2	2	4	4	4

# agg method
# passing dict
genres.agg(
    {
        'Runtime':'mean',
        'IMDB_Rating':'mean',
        'No_of_Votes':'sum',
        'Gross':'sum',
        'Metascore':'min'
    }
)
     
Runtime	IMDB_Rating	No_of_Votes	Gross	Metascore
Genre					
Action	129.046512	7.949419	72282412	3.263226e+10	33.0
Adventure	134.111111	7.937500	22576163	9.496922e+09	41.0
Animation	99.585366	7.930488	21978630	1.463147e+10	61.0
Biography	136.022727	7.938636	24006844	8.276358e+09	48.0
Comedy	112.129032	7.901290	27620327	1.566387e+10	45.0
Crime	126.392523	8.016822	33533615	8.452632e+09	47.0
Drama	124.737024	7.957439	61367304	3.540997e+10	28.0
Family	107.500000	7.800000	551221	4.391106e+08	67.0
Fantasy	85.000000	8.000000	146222	7.827267e+08	NaN
Film-Noir	104.000000	7.966667	367215	1.259105e+08	94.0
Horror	102.090909	7.909091	3742556	1.034649e+09	46.0
Mystery	119.083333	7.975000	4203004	1.256417e+09	52.0
Thriller	108.000000	7.800000	27733	1.755074e+07	81.0
Western	148.250000	8.350000	1289665	5.822151e+07	69.0

# passing list
genres.agg(['min','max','mean','sum'])
     
Runtime	IMDB_Rating	No_of_Votes	Gross	Metascore
min	max	mean	sum	min	max	mean	sum	min	max	mean	sum	min	max	mean	sum	min	max	mean	sum
Genre																				
Action	45	321	129.046512	22196	7.6	9.0	7.949419	1367.3	25312	2303232	420246.581395	72282412	3296.0	936662225.0	1.897224e+08	3.263226e+10	33.0	98.0	73.419580	10499.0
Adventure	88	228	134.111111	9656	7.6	8.6	7.937500	571.5	29999	1512360	313557.819444	22576163	61001.0	874211619.0	1.319017e+08	9.496922e+09	41.0	100.0	78.437500	5020.0
Animation	71	137	99.585366	8166	7.6	8.6	7.930488	650.3	25229	999790	268032.073171	21978630	128985.0	873839108.0	1.784326e+08	1.463147e+10	61.0	96.0	81.093333	6082.0
Biography	93	209	136.022727	11970	7.6	8.9	7.938636	698.6	27254	1213505	272805.045455	24006844	21877.0	753585104.0	9.404952e+07	8.276358e+09	48.0	97.0	76.240506	6023.0
Comedy	68	188	112.129032	17380	7.6	8.6	7.901290	1224.7	26337	939631	178195.658065	27620327	1305.0	886752933.0	1.010572e+08	1.566387e+10	45.0	99.0	78.720000	9840.0
Crime	80	229	126.392523	13524	7.6	9.2	8.016822	857.8	27712	1826188	313398.271028	33533615	6013.0	790482117.0	7.899656e+07	8.452632e+09	47.0	100.0	77.080460	6706.0
Drama	64	242	124.737024	36049	7.6	9.3	7.957439	2299.7	25088	2343110	212343.612457	61367304	3600.0	924558264.0	1.225259e+08	3.540997e+10	28.0	100.0	79.701245	19208.0
Family	100	115	107.500000	215	7.8	7.8	7.800000	15.6	178731	372490	275610.500000	551221	4000000.0	435110554.0	2.195553e+08	4.391106e+08	67.0	91.0	79.000000	158.0
Fantasy	76	94	85.000000	170	7.9	8.1	8.000000	16.0	57428	88794	73111.000000	146222	337574718.0	445151978.0	3.913633e+08	7.827267e+08	NaN	NaN	NaN	0.0
Film-Noir	100	108	104.000000	312	7.8	8.1	7.966667	23.9	59556	158731	122405.000000	367215	449191.0	123353292.0	4.197018e+07	1.259105e+08	94.0	97.0	95.666667	287.0
Horror	71	122	102.090909	1123	7.6	8.5	7.909091	87.0	27007	787806	340232.363636	3742556	89029.0	298791505.0	9.405902e+07	1.034649e+09	46.0	97.0	80.000000	880.0
Mystery	96	138	119.083333	1429	7.6	8.4	7.975000	95.7	33982	1129894	350250.333333	4203004	1035953.0	474203697.0	1.047014e+08	1.256417e+09	52.0	100.0	79.125000	633.0
Thriller	108	108	108.000000	108	7.8	7.8	7.800000	7.8	27733	27733	27733.000000	27733	17550741.0	17550741.0	1.755074e+07	1.755074e+07	81.0	81.0	81.000000	81.0
Western	132	165	148.250000	593	7.8	8.8	8.350000	33.4	65659	688390	322416.250000	1289665	5321508.0	31800000.0	1.455538e+07	5.822151e+07	69.0	90.0	78.250000	313.0

# Adding both the syntax
genres.agg(
    {
        'Runtime':['min','mean'],
        'IMDB_Rating':'mean',
        'No_of_Votes':['sum','max'],
        'Gross':'sum',
        'Metascore':'min'
    }
)
     
Runtime	IMDB_Rating	No_of_Votes	Gross	Metascore
min	mean	mean	sum	max	sum	min
Genre							
Action	45	129.046512	7.949419	72282412	2303232	3.263226e+10	33.0
Adventure	88	134.111111	7.937500	22576163	1512360	9.496922e+09	41.0
Animation	71	99.585366	7.930488	21978630	999790	1.463147e+10	61.0
Biography	93	136.022727	7.938636	24006844	1213505	8.276358e+09	48.0
Comedy	68	112.129032	7.901290	27620327	939631	1.566387e+10	45.0
Crime	80	126.392523	8.016822	33533615	1826188	8.452632e+09	47.0
Drama	64	124.737024	7.957439	61367304	2343110	3.540997e+10	28.0
Family	100	107.500000	7.800000	551221	372490	4.391106e+08	67.0
Fantasy	76	85.000000	8.000000	146222	88794	7.827267e+08	NaN
Film-Noir	100	104.000000	7.966667	367215	158731	1.259105e+08	94.0
Horror	71	102.090909	7.909091	3742556	787806	1.034649e+09	46.0
Mystery	96	119.083333	7.975000	4203004	1129894	1.256417e+09	52.0
Thriller	108	108.000000	7.800000	27733	27733	1.755074e+07	81.0
Western	132	148.250000	8.350000	1289665	688390	5.822151e+07	69.0

# looping on groups
df = pd.DataFrame(columns=movies.columns)
for group,data in genres:
  df = df.append(data[data['IMDB_Rating'] == data['IMDB_Rating'].max()])

df
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
2	The Dark Knight	2008	152	Action	9.0	Christopher Nolan	Christian Bale	2303232	534858444.0	84.0
21	Interstellar	2014	169	Adventure	8.6	Christopher Nolan	Matthew McConaughey	1512360	188020017.0	74.0
23	Sen to Chihiro no kamikakushi	2001	125	Animation	8.6	Hayao Miyazaki	Daveigh Chase	651376	10055859.0	96.0
7	Schindler's List	1993	195	Biography	8.9	Steven Spielberg	Liam Neeson	1213505	96898818.0	94.0
19	Gisaengchung	2019	132	Comedy	8.6	Bong Joon Ho	Kang-ho Song	552778	53367844.0	96.0
26	La vita è bella	1997	116	Comedy	8.6	Roberto Benigni	Roberto Benigni	623629	57598247.0	59.0
1	The Godfather	1972	175	Crime	9.2	Francis Ford Coppola	Marlon Brando	1620367	134966411.0	100.0
0	The Shawshank Redemption	1994	142	Drama	9.3	Frank Darabont	Tim Robbins	2343110	28341469.0	80.0
688	E.T. the Extra-Terrestrial	1982	115	Family	7.8	Steven Spielberg	Henry Thomas	372490	435110554.0	91.0
698	Willy Wonka & the Chocolate Factory	1971	100	Family	7.8	Mel Stuart	Gene Wilder	178731	4000000.0	67.0
321	Das Cabinet des Dr. Caligari	1920	76	Fantasy	8.1	Robert Wiene	Werner Krauss	57428	337574718.0	NaN
309	The Third Man	1949	104	Film-Noir	8.1	Carol Reed	Orson Welles	158731	449191.0	97.0
49	Psycho	1960	109	Horror	8.5	Alfred Hitchcock	Anthony Perkins	604211	32000000.0	97.0
69	Memento	2000	113	Mystery	8.4	Christopher Nolan	Guy Pearce	1125712	25544867.0	80.0
81	Rear Window	1954	112	Mystery	8.4	Alfred Hitchcock	James Stewart	444074	36764313.0	100.0
700	Wait Until Dark	1967	108	Thriller	7.8	Terence Young	Audrey Hepburn	27733	17550741.0	81.0
12	Il buono, il brutto, il cattivo	1966	161	Western	8.8	Sergio Leone	Clint Eastwood	688390	6100000.0	90.0

# split (apply) combine
# apply -> builtin function

genres.apply(min)
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
Genre										
Action	300	1924	45	Action	7.6	Abhishek Chaubey	Aamir Khan	25312	3296.0	33.0
Adventure	2001: A Space Odyssey	1925	88	Adventure	7.6	Akira Kurosawa	Aamir Khan	29999	61001.0	41.0
Animation	Akira	1940	71	Animation	7.6	Adam Elliot	Adrian Molina	25229	128985.0	61.0
Biography	12 Years a Slave	1928	93	Biography	7.6	Adam McKay	Adrien Brody	27254	21877.0	48.0
Comedy	(500) Days of Summer	1921	68	Comedy	7.6	Alejandro G. Iñárritu	Aamir Khan	26337	1305.0	45.0
Crime	12 Angry Men	1931	80	Crime	7.6	Akira Kurosawa	Ajay Devgn	27712	6013.0	47.0
Drama	1917	1925	64	Drama	7.6	Aamir Khan	Abhay Deol	25088	3600.0	28.0
Family	E.T. the Extra-Terrestrial	1971	100	Family	7.8	Mel Stuart	Gene Wilder	178731	4000000.0	67.0
Fantasy	Das Cabinet des Dr. Caligari	1920	76	Fantasy	7.9	F.W. Murnau	Max Schreck	57428	337574718.0	NaN
Film-Noir	Shadow of a Doubt	1941	100	Film-Noir	7.8	Alfred Hitchcock	Humphrey Bogart	59556	449191.0	94.0
Horror	Alien	1933	71	Horror	7.6	Alejandro Amenábar	Anthony Perkins	27007	89029.0	46.0
Mystery	Dark City	1938	96	Mystery	7.6	Alex Proyas	Bernard-Pierre Donnadieu	33982	1035953.0	52.0
Thriller	Wait Until Dark	1967	108	Thriller	7.8	Terence Young	Audrey Hepburn	27733	17550741.0	81.0
Western	Il buono, il brutto, il cattivo	1965	132	Western	7.8	Clint Eastwood	Clint Eastwood	65659	5321508.0	69.0

# find number of movies starting with A for each group

def foo(group):
  return group['Series_Title'].str.startswith('A').sum()

     

genres.apply(foo)
     
Genre
Action       10
Adventure     2
Animation     2
Biography     9
Comedy       14
Crime         4
Drama        21
Family        0
Fantasy       0
Film-Noir     0
Horror        1
Mystery       0
Thriller      0
Western       0
dtype: int64

# find ranking of each movie in the group according to IMDB score

def rank_movie(group):
  group['genre_rank'] = group['IMDB_Rating'].rank(ascending=False)
  return group
     

genres.apply(rank_movie)
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore	genre_rank
0	The Shawshank Redemption	1994	142	Drama	9.3	Frank Darabont	Tim Robbins	2343110	28341469.0	80.0	1.0
1	The Godfather	1972	175	Crime	9.2	Francis Ford Coppola	Marlon Brando	1620367	134966411.0	100.0	1.0
2	The Dark Knight	2008	152	Action	9.0	Christopher Nolan	Christian Bale	2303232	534858444.0	84.0	1.0
3	The Godfather: Part II	1974	202	Crime	9.0	Francis Ford Coppola	Al Pacino	1129952	57300000.0	90.0	2.5
4	12 Angry Men	1957	96	Crime	9.0	Sidney Lumet	Henry Fonda	689845	4360000.0	96.0	2.5
...	...	...	...	...	...	...	...	...	...	...	...
995	Breakfast at Tiffany's	1961	115	Comedy	7.6	Blake Edwards	Audrey Hepburn	166544	679874270.0	76.0	147.0
996	Giant	1956	201	Drama	7.6	George Stevens	Elizabeth Taylor	34075	195217415.0	84.0	272.5
997	From Here to Eternity	1953	118	Drama	7.6	Fred Zinnemann	Burt Lancaster	43374	30500000.0	85.0	272.5
998	Lifeboat	1944	97	Drama	7.6	Alfred Hitchcock	Tallulah Bankhead	26471	852142728.0	78.0	272.5
999	The 39 Steps	1935	86	Crime	7.6	Alfred Hitchcock	Robert Donat	51853	302787539.0	93.0	101.0
1000 rows × 11 columns


# find normalized IMDB rating group wise

def normal(group):
  group['norm_rating'] = (group['IMDB_Rating'] - group['IMDB_Rating'].min())/(group['IMDB_Rating'].max() - group['IMDB_Rating'].min())
  return group

genres.apply(normal)
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore	norm_rating
0	The Shawshank Redemption	1994	142	Drama	9.3	Frank Darabont	Tim Robbins	2343110	28341469.0	80.0	1.000
1	The Godfather	1972	175	Crime	9.2	Francis Ford Coppola	Marlon Brando	1620367	134966411.0	100.0	1.000
2	The Dark Knight	2008	152	Action	9.0	Christopher Nolan	Christian Bale	2303232	534858444.0	84.0	1.000
3	The Godfather: Part II	1974	202	Crime	9.0	Francis Ford Coppola	Al Pacino	1129952	57300000.0	90.0	0.875
4	12 Angry Men	1957	96	Crime	9.0	Sidney Lumet	Henry Fonda	689845	4360000.0	96.0	0.875
...	...	...	...	...	...	...	...	...	...	...	...
995	Breakfast at Tiffany's	1961	115	Comedy	7.6	Blake Edwards	Audrey Hepburn	166544	679874270.0	76.0	0.000
996	Giant	1956	201	Drama	7.6	George Stevens	Elizabeth Taylor	34075	195217415.0	84.0	0.000
997	From Here to Eternity	1953	118	Drama	7.6	Fred Zinnemann	Burt Lancaster	43374	30500000.0	85.0	0.000
998	Lifeboat	1944	97	Drama	7.6	Alfred Hitchcock	Tallulah Bankhead	26471	852142728.0	78.0	0.000
999	The 39 Steps	1935	86	Crime	7.6	Alfred Hitchcock	Robert Donat	51853	302787539.0	93.0	0.000
1000 rows × 11 columns


# groupby on multiple cols
duo = movies.groupby(['Director','Star1'])
duo
# size
duo.size()
# get_group
duo.get_group(('Aamir Khan','Amole Gupte'))
     
Series_Title	Released_Year	Runtime	Genre	IMDB_Rating	Director	Star1	No_of_Votes	Gross	Metascore
65	Taare Zameen Par	2007	165	Drama	8.4	Aamir Khan	Amole Gupte	168895	1223869.0	NaN

# find the most earning actor->director combo
duo['Gross'].sum().sort_values(ascending=False).head(1)
     
Director        Star1         
Akira Kurosawa  Toshirô Mifune    2.999877e+09
Name: Gross, dtype: float64

# find the best(in-terms of metascore(avg)) actor->genre combo
movies.groupby(['Star1','Genre'])['Metascore'].mean().reset_index().sort_values('Metascore',ascending=False).head(1)
     
Star1	Genre	Metascore
230	Ellar Coltrane	Drama	100.0

# agg on multiple groupby
duo.agg(['min','max','mean'])
     
Runtime	IMDB_Rating	No_of_Votes	Gross	Metascore
min	max	mean	min	max	mean	min	max	mean	min	max	mean	min	max	mean
Director	Star1															
Aamir Khan	Amole Gupte	165	165	165.0	8.4	8.4	8.4	168895	168895	168895.0	1223869.0	1223869.0	1223869.0	NaN	NaN	NaN
Aaron Sorkin	Eddie Redmayne	129	129	129.0	7.8	7.8	7.8	89896	89896	89896.0	853090410.0	853090410.0	853090410.0	77.0	77.0	77.0
Abdellatif Kechiche	Léa Seydoux	180	180	180.0	7.7	7.7	7.7	138741	138741	138741.0	2199675.0	2199675.0	2199675.0	89.0	89.0	89.0
Abhishek Chaubey	Shahid Kapoor	148	148	148.0	7.8	7.8	7.8	27175	27175	27175.0	218428303.0	218428303.0	218428303.0	NaN	NaN	NaN
Abhishek Kapoor	Amit Sadh	130	130	130.0	7.7	7.7	7.7	32628	32628	32628.0	1122527.0	1122527.0	1122527.0	40.0	40.0	40.0
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
Zaza Urushadze	Lembit Ulfsak	87	87	87.0	8.2	8.2	8.2	40382	40382	40382.0	144501.0	144501.0	144501.0	73.0	73.0	73.0
Zoya Akhtar	Hrithik Roshan	155	155	155.0	8.1	8.1	8.1	67927	67927	67927.0	3108485.0	3108485.0	3108485.0	NaN	NaN	NaN
Vijay Varma	154	154	154.0	8.0	8.0	8.0	31886	31886	31886.0	5566534.0	5566534.0	5566534.0	65.0	65.0	65.0
Çagan Irmak	Çetin Tekindor	112	112	112.0	8.3	8.3	8.3	78925	78925	78925.0	461855363.0	461855363.0	461855363.0	NaN	NaN	NaN
Ömer Faruk Sorak	Cem Yilmaz	127	127	127.0	8.0	8.0	8.0	56960	56960	56960.0	196206077.0	196206077.0	196206077.0	NaN	NaN	NaN
898 rows × 15 columns

Excercise

ipl = pd.read_csv('/content/deliveries.csv')
ipl.head()
     
match_id	inning	batting_team	bowling_team	over	ball	batsman	non_striker	bowler	is_super_over	...	bye_runs	legbye_runs	noball_runs	penalty_runs	batsman_runs	extra_runs	total_runs	player_dismissed	dismissal_kind	fielder
0	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	1	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
1	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	2	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
2	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	3	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	4	0	4	NaN	NaN	NaN
3	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	4	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
4	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	5	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	2	2	NaN	NaN	NaN
5 rows × 21 columns


ipl.shape
     
(179078, 21)

# find the top 10 batsman in terms of runs
ipl.groupby('batsman')['batsman_runs'].sum().sort_values(ascending=False).head(10)
     
batsman
V Kohli           5434
SK Raina          5415
RG Sharma         4914
DA Warner         4741
S Dhawan          4632
CH Gayle          4560
MS Dhoni          4477
RV Uthappa        4446
AB de Villiers    4428
G Gambhir         4223
Name: batsman_runs, dtype: int64

# find the batsman with max no of sixes
six = ipl[ipl['batsman_runs'] == 6]

six.groupby('batsman')['batsman'].count().sort_values(ascending=False).head(1).index[0]
     
'CH Gayle'

# find batsman with most number of 4's and 6's in last 5 overs
temp_df = ipl[ipl['over'] > 15]
temp_df = temp_df[(temp_df['batsman_runs'] == 4) | (temp_df['batsman_runs'] == 6)]
temp_df.groupby('batsman')['batsman'].count().sort_values(ascending=False).head(1).index[0]
     
'MS Dhoni'

# find V Kohli's record against all teams
temp_df = ipl[ipl['batsman'] == 'V Kohli']

temp_df.groupby('bowling_team')['batsman_runs'].sum().reset_index()
     
bowling_team	batsman_runs
0	Chennai Super Kings	749
1	Deccan Chargers	306
2	Delhi Capitals	66
3	Delhi Daredevils	763
4	Gujarat Lions	283
5	Kings XI Punjab	636
6	Kochi Tuskers Kerala	50
7	Kolkata Knight Riders	675
8	Mumbai Indians	628
9	Pune Warriors	128
10	Rajasthan Royals	370
11	Rising Pune Supergiant	83
12	Rising Pune Supergiants	188
13	Sunrisers Hyderabad	509

# Create a function that can return the highest score of any batsman
temp_df = ipl[ipl['batsman'] == 'V Kohli']
temp_df.groupby('match_id')['batsman_runs'].sum().sort_values(ascending=False).head(1).values[0]
     
113

def highest(batsman):
  temp_df = ipl[ipl['batsman'] == batsman]
  return temp_df.groupby('match_id')['batsman_runs'].sum().sort_values(ascending=False).head(1).values[0]

     

highest('DA Warner')
     
126


     
####################################################################################



import pandas as pd
import numpy as np
     

courses = pd.read_csv('/content/courses.csv')
students = pd.read_csv('/content/students.csv')
nov = pd.read_csv('/content/reg-month1.csv')
dec = pd.read_csv('/content/reg-month2.csv')

matches = pd.read_csv('/content/matches.csv')
delivery = pd.read_csv('/content/deliveries.csv')
     

dec
     
student_id	course_id
0	3	5
1	16	7
2	12	10
3	12	1
4	14	9
5	7	7
6	7	2
7	16	3
8	17	10
9	11	8
10	14	6
11	12	5
12	12	7
13	18	8
14	1	10
15	1	9
16	2	5
17	7	6
18	22	5
19	22	6
20	23	9
21	23	5
22	14	4
23	14	1
24	11	10
25	42	9
26	50	8
27	38	1

# pd.concat
# df.concat
# ignore_index
# df.append
# mullitindex -> fetch using iloc
# concat dataframes horizontally
     

regs = pd.concat([nov,dec],ignore_index=True)
regs
     
student_id	course_id
0	23	1
1	15	5
2	18	6
3	23	4
4	16	9
5	18	1
6	1	1
7	7	8
8	22	3
9	15	1
10	19	4
11	1	6
12	7	10
13	11	7
14	13	3
15	24	4
16	21	1
17	16	5
18	23	3
19	17	7
20	23	6
21	25	1
22	19	2
23	25	10
24	3	3
25	3	5
26	16	7
27	12	10
28	12	1
29	14	9
30	7	7
31	7	2
32	16	3
33	17	10
34	11	8
35	14	6
36	12	5
37	12	7
38	18	8
39	1	10
40	1	9
41	2	5
42	7	6
43	22	5
44	22	6
45	23	9
46	23	5
47	14	4
48	14	1
49	11	10
50	42	9
51	50	8
52	38	1

nov.append(dec,ignore_index=True)
     
student_id	course_id
0	23	1
1	15	5
2	18	6
3	23	4
4	16	9
5	18	1
6	1	1
7	7	8
8	22	3
9	15	1
10	19	4
11	1	6
12	7	10
13	11	7
14	13	3
15	24	4
16	21	1
17	16	5
18	23	3
19	17	7
20	23	6
21	25	1
22	19	2
23	25	10
24	3	3
25	3	5
26	16	7
27	12	10
28	12	1
29	14	9
30	7	7
31	7	2
32	16	3
33	17	10
34	11	8
35	14	6
36	12	5
37	12	7
38	18	8
39	1	10
40	1	9
41	2	5
42	7	6
43	22	5
44	22	6
45	23	9
46	23	5
47	14	4
48	14	1
49	11	10
50	42	9
51	50	8
52	38	1

multi = pd.concat([nov,dec],keys=['Nov','Dec'])
# Multiindex DataFrame
multi.loc[('Dec',4)]
     
student_id    14
course_id      9
Name: (Dec, 4), dtype: int64

pd.concat([nov,dec],axis=1)
     
student_id	course_id	student_id	course_id
0	23.0	1.0	3	5
1	15.0	5.0	16	7
2	18.0	6.0	12	10
3	23.0	4.0	12	1
4	16.0	9.0	14	9
5	18.0	1.0	7	7
6	1.0	1.0	7	2
7	7.0	8.0	16	3
8	22.0	3.0	17	10
9	15.0	1.0	11	8
10	19.0	4.0	14	6
11	1.0	6.0	12	5
12	7.0	10.0	12	7
13	11.0	7.0	18	8
14	13.0	3.0	1	10
15	24.0	4.0	1	9
16	21.0	1.0	2	5
17	16.0	5.0	7	6
18	23.0	3.0	22	5
19	17.0	7.0	22	6
20	23.0	6.0	23	9
21	25.0	1.0	23	5
22	19.0	2.0	14	4
23	25.0	10.0	14	1
24	3.0	3.0	11	10
25	NaN	NaN	42	9
26	NaN	NaN	50	8
27	NaN	NaN	38	1

# inner join
students.merge(regs,how='inner',on='student_id')
     
student_id	name	partner	course_id
0	1	Kailash Harjo	23	1
1	1	Kailash Harjo	23	6
2	1	Kailash Harjo	23	10
3	1	Kailash Harjo	23	9
4	2	Esha Butala	1	5
5	3	Parveen Bhalla	3	3
6	3	Parveen Bhalla	3	5
7	7	Tarun Thaker	9	8
8	7	Tarun Thaker	9	10
9	7	Tarun Thaker	9	7
10	7	Tarun Thaker	9	2
11	7	Tarun Thaker	9	6
12	11	David Mukhopadhyay	20	7
13	11	David Mukhopadhyay	20	8
14	11	David Mukhopadhyay	20	10
15	12	Radha Dutt	19	10
16	12	Radha Dutt	19	1
17	12	Radha Dutt	19	5
18	12	Radha Dutt	19	7
19	13	Munni Varghese	24	3
20	14	Pranab Natarajan	22	9
21	14	Pranab Natarajan	22	6
22	14	Pranab Natarajan	22	4
23	14	Pranab Natarajan	22	1
24	15	Preet Sha	16	5
25	15	Preet Sha	16	1
26	16	Elias Dodiya	25	9
27	16	Elias Dodiya	25	5
28	16	Elias Dodiya	25	7
29	16	Elias Dodiya	25	3
30	17	Yasmin Palan	7	7
31	17	Yasmin Palan	7	10
32	18	Fardeen Mahabir	13	6
33	18	Fardeen Mahabir	13	1
34	18	Fardeen Mahabir	13	8
35	19	Qabeel Raman	12	4
36	19	Qabeel Raman	12	2
37	21	Seema Kota	15	1
38	22	Yash Sethi	21	3
39	22	Yash Sethi	21	5
40	22	Yash Sethi	21	6
41	23	Chhavi Lachman	18	1
42	23	Chhavi Lachman	18	4
43	23	Chhavi Lachman	18	3
44	23	Chhavi Lachman	18	6
45	23	Chhavi Lachman	18	9
46	23	Chhavi Lachman	18	5
47	24	Radhika Suri	17	4
48	25	Shashank D’Alia	2	1
49	25	Shashank D’Alia	2	10

# left join
courses.merge(regs,how='left',on='course_id')
     
course_id	course_name	price	student_id
0	1	python	2499	23.0
1	1	python	2499	18.0
2	1	python	2499	1.0
3	1	python	2499	15.0
4	1	python	2499	21.0
5	1	python	2499	25.0
6	1	python	2499	12.0
7	1	python	2499	14.0
8	1	python	2499	38.0
9	2	sql	3499	19.0
10	2	sql	3499	7.0
11	3	data analysis	4999	22.0
12	3	data analysis	4999	13.0
13	3	data analysis	4999	23.0
14	3	data analysis	4999	3.0
15	3	data analysis	4999	16.0
16	4	machine learning	9999	23.0
17	4	machine learning	9999	19.0
18	4	machine learning	9999	24.0
19	4	machine learning	9999	14.0
20	5	tableau	2499	15.0
21	5	tableau	2499	16.0
22	5	tableau	2499	3.0
23	5	tableau	2499	12.0
24	5	tableau	2499	2.0
25	5	tableau	2499	22.0
26	5	tableau	2499	23.0
27	6	power bi	1899	18.0
28	6	power bi	1899	1.0
29	6	power bi	1899	23.0
30	6	power bi	1899	14.0
31	6	power bi	1899	7.0
32	6	power bi	1899	22.0
33	7	ms sxcel	1599	11.0
34	7	ms sxcel	1599	17.0
35	7	ms sxcel	1599	16.0
36	7	ms sxcel	1599	7.0
37	7	ms sxcel	1599	12.0
38	8	pandas	1099	7.0
39	8	pandas	1099	11.0
40	8	pandas	1099	18.0
41	8	pandas	1099	50.0
42	9	plotly	699	16.0
43	9	plotly	699	14.0
44	9	plotly	699	1.0
45	9	plotly	699	23.0
46	9	plotly	699	42.0
47	10	pyspark	2499	7.0
48	10	pyspark	2499	25.0
49	10	pyspark	2499	12.0
50	10	pyspark	2499	17.0
51	10	pyspark	2499	1.0
52	10	pyspark	2499	11.0
53	11	Numpy	699	NaN
54	12	C++	1299	NaN

# right join
temp_df = pd.DataFrame({
    'student_id':[26,27,28],
    'name':['Nitish','Ankit','Rahul'],
    'partner':[28,26,17]
})

students = pd.concat([students,temp_df],ignore_index=True)
     

students.tail()
     
student_id	name	partner
23	24	Radhika Suri	17
24	25	Shashank D’Alia	2
25	26	Nitish	28
26	27	Ankit	26
27	28	Rahul	17

students.merge(regs,how='right',on='student_id')
     
student_id	name	partner	course_id
0	23	Chhavi Lachman	18.0	1
1	15	Preet Sha	16.0	5
2	18	Fardeen Mahabir	13.0	6
3	23	Chhavi Lachman	18.0	4
4	16	Elias Dodiya	25.0	9
5	18	Fardeen Mahabir	13.0	1
6	1	Kailash Harjo	23.0	1
7	7	Tarun Thaker	9.0	8
8	22	Yash Sethi	21.0	3
9	15	Preet Sha	16.0	1
10	19	Qabeel Raman	12.0	4
11	1	Kailash Harjo	23.0	6
12	7	Tarun Thaker	9.0	10
13	11	David Mukhopadhyay	20.0	7
14	13	Munni Varghese	24.0	3
15	24	Radhika Suri	17.0	4
16	21	Seema Kota	15.0	1
17	16	Elias Dodiya	25.0	5
18	23	Chhavi Lachman	18.0	3
19	17	Yasmin Palan	7.0	7
20	23	Chhavi Lachman	18.0	6
21	25	Shashank D’Alia	2.0	1
22	19	Qabeel Raman	12.0	2
23	25	Shashank D’Alia	2.0	10
24	3	Parveen Bhalla	3.0	3
25	3	Parveen Bhalla	3.0	5
26	16	Elias Dodiya	25.0	7
27	12	Radha Dutt	19.0	10
28	12	Radha Dutt	19.0	1
29	14	Pranab Natarajan	22.0	9
30	7	Tarun Thaker	9.0	7
31	7	Tarun Thaker	9.0	2
32	16	Elias Dodiya	25.0	3
33	17	Yasmin Palan	7.0	10
34	11	David Mukhopadhyay	20.0	8
35	14	Pranab Natarajan	22.0	6
36	12	Radha Dutt	19.0	5
37	12	Radha Dutt	19.0	7
38	18	Fardeen Mahabir	13.0	8
39	1	Kailash Harjo	23.0	10
40	1	Kailash Harjo	23.0	9
41	2	Esha Butala	1.0	5
42	7	Tarun Thaker	9.0	6
43	22	Yash Sethi	21.0	5
44	22	Yash Sethi	21.0	6
45	23	Chhavi Lachman	18.0	9
46	23	Chhavi Lachman	18.0	5
47	14	Pranab Natarajan	22.0	4
48	14	Pranab Natarajan	22.0	1
49	11	David Mukhopadhyay	20.0	10
50	42	NaN	NaN	9
51	50	NaN	NaN	8
52	38	NaN	NaN	1

regs.merge(students,how='left',on='student_id')
     
student_id	course_id	name	partner
0	23	1	Chhavi Lachman	18.0
1	15	5	Preet Sha	16.0
2	18	6	Fardeen Mahabir	13.0
3	23	4	Chhavi Lachman	18.0
4	16	9	Elias Dodiya	25.0
5	18	1	Fardeen Mahabir	13.0
6	1	1	Kailash Harjo	23.0
7	7	8	Tarun Thaker	9.0
8	22	3	Yash Sethi	21.0
9	15	1	Preet Sha	16.0
10	19	4	Qabeel Raman	12.0
11	1	6	Kailash Harjo	23.0
12	7	10	Tarun Thaker	9.0
13	11	7	David Mukhopadhyay	20.0
14	13	3	Munni Varghese	24.0
15	24	4	Radhika Suri	17.0
16	21	1	Seema Kota	15.0
17	16	5	Elias Dodiya	25.0
18	23	3	Chhavi Lachman	18.0
19	17	7	Yasmin Palan	7.0
20	23	6	Chhavi Lachman	18.0
21	25	1	Shashank D’Alia	2.0
22	19	2	Qabeel Raman	12.0
23	25	10	Shashank D’Alia	2.0
24	3	3	Parveen Bhalla	3.0
25	3	5	Parveen Bhalla	3.0
26	16	7	Elias Dodiya	25.0
27	12	10	Radha Dutt	19.0
28	12	1	Radha Dutt	19.0
29	14	9	Pranab Natarajan	22.0
30	7	7	Tarun Thaker	9.0
31	7	2	Tarun Thaker	9.0
32	16	3	Elias Dodiya	25.0
33	17	10	Yasmin Palan	7.0
34	11	8	David Mukhopadhyay	20.0
35	14	6	Pranab Natarajan	22.0
36	12	5	Radha Dutt	19.0
37	12	7	Radha Dutt	19.0
38	18	8	Fardeen Mahabir	13.0
39	1	10	Kailash Harjo	23.0
40	1	9	Kailash Harjo	23.0
41	2	5	Esha Butala	1.0
42	7	6	Tarun Thaker	9.0
43	22	5	Yash Sethi	21.0
44	22	6	Yash Sethi	21.0
45	23	9	Chhavi Lachman	18.0
46	23	5	Chhavi Lachman	18.0
47	14	4	Pranab Natarajan	22.0
48	14	1	Pranab Natarajan	22.0
49	11	10	David Mukhopadhyay	20.0
50	42	9	NaN	NaN
51	50	8	NaN	NaN
52	38	1	NaN	NaN

# outer join
students.merge(regs,how='outer',on='student_id').tail(10)
     
student_id	name	partner	course_id
53	23	Chhavi Lachman	18.0	5.0
54	24	Radhika Suri	17.0	4.0
55	25	Shashank D’Alia	2.0	1.0
56	25	Shashank D’Alia	2.0	10.0
57	26	Nitish	28.0	NaN
58	27	Ankit	26.0	NaN
59	28	Rahul	17.0	NaN
60	42	NaN	NaN	9.0
61	50	NaN	NaN	8.0
62	38	NaN	NaN	1.0

# 1. find total revenue generated
total = regs.merge(courses,how='inner',on='course_id')['price'].sum()
total
     
154247

# 2. find month by month revenue
temp_df = pd.concat([nov,dec],keys=['Nov','Dec']).reset_index()
temp_df.merge(courses,on='course_id').groupby('level_0')['price'].sum()
     
level_0
Dec    65072
Nov    89175
Name: price, dtype: int64

# 3. Print the registration table
# cols -> name -> course -> price
regs.merge(students,on='student_id').merge(courses,on='course_id')[['name','course_name','price']]
     
name	course_name	price
0	Chhavi Lachman	python	2499
1	Preet Sha	python	2499
2	Fardeen Mahabir	python	2499
3	Kailash Harjo	python	2499
4	Seema Kota	python	2499
5	Shashank D’Alia	python	2499
6	Radha Dutt	python	2499
7	Pranab Natarajan	python	2499
8	Chhavi Lachman	machine learning	9999
9	Qabeel Raman	machine learning	9999
10	Radhika Suri	machine learning	9999
11	Pranab Natarajan	machine learning	9999
12	Chhavi Lachman	data analysis	4999
13	Elias Dodiya	data analysis	4999
14	Yash Sethi	data analysis	4999
15	Munni Varghese	data analysis	4999
16	Parveen Bhalla	data analysis	4999
17	Chhavi Lachman	power bi	1899
18	Fardeen Mahabir	power bi	1899
19	Kailash Harjo	power bi	1899
20	Tarun Thaker	power bi	1899
21	Yash Sethi	power bi	1899
22	Pranab Natarajan	power bi	1899
23	Chhavi Lachman	plotly	699
24	Elias Dodiya	plotly	699
25	Kailash Harjo	plotly	699
26	Pranab Natarajan	plotly	699
27	Chhavi Lachman	tableau	2499
28	Preet Sha	tableau	2499
29	Elias Dodiya	tableau	2499
30	Yash Sethi	tableau	2499
31	Parveen Bhalla	tableau	2499
32	Radha Dutt	tableau	2499
33	Esha Butala	tableau	2499
34	Fardeen Mahabir	pandas	1099
35	Tarun Thaker	pandas	1099
36	David Mukhopadhyay	pandas	1099
37	Elias Dodiya	ms sxcel	1599
38	Tarun Thaker	ms sxcel	1599
39	David Mukhopadhyay	ms sxcel	1599
40	Yasmin Palan	ms sxcel	1599
41	Radha Dutt	ms sxcel	1599
42	Kailash Harjo	pyspark	2499
43	Tarun Thaker	pyspark	2499
44	David Mukhopadhyay	pyspark	2499
45	Yasmin Palan	pyspark	2499
46	Shashank D’Alia	pyspark	2499
47	Radha Dutt	pyspark	2499
48	Tarun Thaker	sql	3499
49	Qabeel Raman	sql	3499

# 4. Plot bar chart for revenue/course
regs.merge(courses,on='course_id').groupby('course_name')['price'].sum().plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f56b73cb2e0>


# 5. find students who enrolled in both the months
common_student_id = np.intersect1d(nov['student_id'],dec['student_id'])
common_student_id
     
array([ 1,  3,  7, 11, 16, 17, 18, 22, 23])

students[students['student_id'].isin(common_student_id)]
     
student_id	name	partner
0	1	Kailash Harjo	23
2	3	Parveen Bhalla	3
6	7	Tarun Thaker	9
10	11	David Mukhopadhyay	20
15	16	Elias Dodiya	25
16	17	Yasmin Palan	7
17	18	Fardeen Mahabir	13
21	22	Yash Sethi	21
22	23	Chhavi Lachman	18

# 6. find course that got no enrollment
# courses['course_id']
# regs['course_id']

course_id_list = np.setdiff1d(courses['course_id'],regs['course_id'])
courses[courses['course_id'].isin(course_id_list)]
     
course_id	course_name	price
10	11	Numpy	699
11	12	C++	1299

# 7. find students who did not enroll into any courses
student_id_list = np.setdiff1d(students['student_id'],regs['student_id'])
students[students['student_id'].isin(student_id_list)].shape[0]

(10/28)*100
     
35.714285714285715

students
     
student_id	name	partner
0	1	Kailash Harjo	23
1	2	Esha Butala	1
2	3	Parveen Bhalla	3
3	4	Marlo Dugal	14
4	5	Kusum Bahri	6
5	6	Lakshmi Contractor	10
6	7	Tarun Thaker	9
7	8	Radheshyam Dey	5
8	9	Nitika Chatterjee	4
9	10	Aayushman Sant	8
10	11	David Mukhopadhyay	20
11	12	Radha Dutt	19
12	13	Munni Varghese	24
13	14	Pranab Natarajan	22
14	15	Preet Sha	16
15	16	Elias Dodiya	25
16	17	Yasmin Palan	7
17	18	Fardeen Mahabir	13
18	19	Qabeel Raman	12
19	20	Hanuman Hegde	11
20	21	Seema Kota	15
21	22	Yash Sethi	21
22	23	Chhavi Lachman	18
23	24	Radhika Suri	17
24	25	Shashank D’Alia	2
25	26	Nitish	28
26	27	Ankit	26
27	28	Rahul	17

# 8. Print student name -> partner name for all enrolled students
# self join
students.merge(students,how='inner',left_on='partner',right_on='student_id')[['name_x','name_y']]
     
name_x	name_y
0	Kailash Harjo	Chhavi Lachman
1	Esha Butala	Kailash Harjo
2	Parveen Bhalla	Parveen Bhalla
3	Marlo Dugal	Pranab Natarajan
4	Kusum Bahri	Lakshmi Contractor
5	Lakshmi Contractor	Aayushman Sant
6	Tarun Thaker	Nitika Chatterjee
7	Radheshyam Dey	Kusum Bahri
8	Nitika Chatterjee	Marlo Dugal
9	Aayushman Sant	Radheshyam Dey
10	David Mukhopadhyay	Hanuman Hegde
11	Radha Dutt	Qabeel Raman
12	Munni Varghese	Radhika Suri
13	Pranab Natarajan	Yash Sethi
14	Preet Sha	Elias Dodiya
15	Elias Dodiya	Shashank D’Alia
16	Yasmin Palan	Tarun Thaker
17	Fardeen Mahabir	Munni Varghese
18	Qabeel Raman	Radha Dutt
19	Hanuman Hegde	David Mukhopadhyay
20	Seema Kota	Preet Sha
21	Yash Sethi	Seema Kota
22	Chhavi Lachman	Fardeen Mahabir
23	Radhika Suri	Yasmin Palan
24	Rahul	Yasmin Palan
25	Shashank D’Alia	Esha Butala
26	Nitish	Rahul
27	Ankit	Nitish

# 9. find top 3 students who did most number enrollments
regs.merge(students,on='student_id').groupby(['student_id','name'])['name'].count().sort_values(ascending=False).head(3)
     
student_id  name          
23          Chhavi Lachman    6
7           Tarun Thaker      5
1           Kailash Harjo     4
Name: name, dtype: int64

# 10. find top 3 students who spent most amount of money on courses
regs.merge(students,on='student_id').merge(courses,on='course_id').groupby(['student_id','name'])['price'].sum().sort_values(ascending=False).head(3)
     
student_id  name            
23          Chhavi Lachman      22594
14          Pranab Natarajan    15096
19          Qabeel Raman        13498
Name: price, dtype: int64

# Alternate syntax for merge
# students.merge(regs)

pd.merge(students,regs,how='inner',on='student_id')
     
student_id	name	partner	course_id
0	1	Kailash Harjo	23	1
1	1	Kailash Harjo	23	6
2	1	Kailash Harjo	23	10
3	1	Kailash Harjo	23	9
4	2	Esha Butala	1	5
5	3	Parveen Bhalla	3	3
6	3	Parveen Bhalla	3	5
7	7	Tarun Thaker	9	8
8	7	Tarun Thaker	9	10
9	7	Tarun Thaker	9	7
10	7	Tarun Thaker	9	2
11	7	Tarun Thaker	9	6
12	11	David Mukhopadhyay	20	7
13	11	David Mukhopadhyay	20	8
14	11	David Mukhopadhyay	20	10
15	12	Radha Dutt	19	10
16	12	Radha Dutt	19	1
17	12	Radha Dutt	19	5
18	12	Radha Dutt	19	7
19	13	Munni Varghese	24	3
20	14	Pranab Natarajan	22	9
21	14	Pranab Natarajan	22	6
22	14	Pranab Natarajan	22	4
23	14	Pranab Natarajan	22	1
24	15	Preet Sha	16	5
25	15	Preet Sha	16	1
26	16	Elias Dodiya	25	9
27	16	Elias Dodiya	25	5
28	16	Elias Dodiya	25	7
29	16	Elias Dodiya	25	3
30	17	Yasmin Palan	7	7
31	17	Yasmin Palan	7	10
32	18	Fardeen Mahabir	13	6
33	18	Fardeen Mahabir	13	1
34	18	Fardeen Mahabir	13	8
35	19	Qabeel Raman	12	4
36	19	Qabeel Raman	12	2
37	21	Seema Kota	15	1
38	22	Yash Sethi	21	3
39	22	Yash Sethi	21	5
40	22	Yash Sethi	21	6
41	23	Chhavi Lachman	18	1
42	23	Chhavi Lachman	18	4
43	23	Chhavi Lachman	18	3
44	23	Chhavi Lachman	18	6
45	23	Chhavi Lachman	18	9
46	23	Chhavi Lachman	18	5
47	24	Radhika Suri	17	4
48	25	Shashank D’Alia	2	1
49	25	Shashank D’Alia	2	10

# IPL Problems

# find top 3 studiums with highest sixes/match ratio
# find orange cap holder of all the seasons
     

matches
     
id	season	city	date	team1	team2	toss_winner	toss_decision	result	dl_applied	winner	win_by_runs	win_by_wickets	player_of_match	venue	umpire1	umpire2	umpire3
0	1	2017	Hyderabad	2017-04-05	Sunrisers Hyderabad	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Sunrisers Hyderabad	35	0	Yuvraj Singh	Rajiv Gandhi International Stadium, Uppal	AY Dandekar	NJ Llong	NaN
1	2	2017	Pune	2017-04-06	Mumbai Indians	Rising Pune Supergiant	Rising Pune Supergiant	field	normal	0	Rising Pune Supergiant	0	7	SPD Smith	Maharashtra Cricket Association Stadium	A Nand Kishore	S Ravi	NaN
2	3	2017	Rajkot	2017-04-07	Gujarat Lions	Kolkata Knight Riders	Kolkata Knight Riders	field	normal	0	Kolkata Knight Riders	0	10	CA Lynn	Saurashtra Cricket Association Stadium	Nitin Menon	CK Nandan	NaN
3	4	2017	Indore	2017-04-08	Rising Pune Supergiant	Kings XI Punjab	Kings XI Punjab	field	normal	0	Kings XI Punjab	0	6	GJ Maxwell	Holkar Cricket Stadium	AK Chaudhary	C Shamshuddin	NaN
4	5	2017	Bangalore	2017-04-08	Royal Challengers Bangalore	Delhi Daredevils	Royal Challengers Bangalore	bat	normal	0	Royal Challengers Bangalore	15	0	KM Jadhav	M Chinnaswamy Stadium	NaN	NaN	NaN
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
631	632	2016	Raipur	2016-05-22	Delhi Daredevils	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Royal Challengers Bangalore	0	6	V Kohli	Shaheed Veer Narayan Singh International Stadium	A Nand Kishore	BNJ Oxenford	NaN
632	633	2016	Bangalore	2016-05-24	Gujarat Lions	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Royal Challengers Bangalore	0	4	AB de Villiers	M Chinnaswamy Stadium	AK Chaudhary	HDPK Dharmasena	NaN
633	634	2016	Delhi	2016-05-25	Sunrisers Hyderabad	Kolkata Knight Riders	Kolkata Knight Riders	field	normal	0	Sunrisers Hyderabad	22	0	MC Henriques	Feroz Shah Kotla	M Erasmus	C Shamshuddin	NaN
634	635	2016	Delhi	2016-05-27	Gujarat Lions	Sunrisers Hyderabad	Sunrisers Hyderabad	field	normal	0	Sunrisers Hyderabad	0	4	DA Warner	Feroz Shah Kotla	M Erasmus	CK Nandan	NaN
635	636	2016	Bangalore	2016-05-29	Sunrisers Hyderabad	Royal Challengers Bangalore	Sunrisers Hyderabad	bat	normal	0	Sunrisers Hyderabad	8	0	BCJ Cutting	M Chinnaswamy Stadium	HDPK Dharmasena	BNJ Oxenford	NaN
636 rows × 18 columns


delivery
     
match_id	inning	batting_team	bowling_team	over	ball	batsman	non_striker	bowler	is_super_over	...	bye_runs	legbye_runs	noball_runs	penalty_runs	batsman_runs	extra_runs	total_runs	player_dismissed	dismissal_kind	fielder
0	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	1	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
1	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	2	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
2	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	3	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	4	0	4	NaN	NaN	NaN
3	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	4	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	0	0	NaN	NaN	NaN
4	1	1	Sunrisers Hyderabad	Royal Challengers Bangalore	1	5	DA Warner	S Dhawan	TS Mills	0	...	0	0	0	0	0	2	2	NaN	NaN	NaN
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
150455	636	2	Royal Challengers Bangalore	Sunrisers Hyderabad	20	2	Sachin Baby	CJ Jordan	B Kumar	0	...	0	0	0	0	2	0	2	NaN	NaN	NaN
150456	636	2	Royal Challengers Bangalore	Sunrisers Hyderabad	20	3	Sachin Baby	CJ Jordan	B Kumar	0	...	0	0	0	0	0	0	0	CJ Jordan	run out	NV Ojha
150457	636	2	Royal Challengers Bangalore	Sunrisers Hyderabad	20	4	Iqbal Abdulla	Sachin Baby	B Kumar	0	...	0	1	0	0	0	1	1	NaN	NaN	NaN
150458	636	2	Royal Challengers Bangalore	Sunrisers Hyderabad	20	5	Sachin Baby	Iqbal Abdulla	B Kumar	0	...	0	0	0	0	1	0	1	NaN	NaN	NaN
150459	636	2	Royal Challengers Bangalore	Sunrisers Hyderabad	20	6	Iqbal Abdulla	Sachin Baby	B Kumar	0	...	0	0	0	0	4	0	4	NaN	NaN	NaN
150460 rows × 21 columns


temp_df = delivery.merge(matches,left_on='match_id',right_on='id')
     

six_df = temp_df[temp_df['batsman_runs'] == 6]
     

# stadium -> sixes
num_sixes = six_df.groupby('venue')['venue'].count()
     

num_matches = matches['venue'].value_counts()
     

(num_sixes/num_matches).sort_values(ascending=False).head(10)
     
Holkar Cricket Stadium                                 17.600000
M Chinnaswamy Stadium                                  13.227273
Sharjah Cricket Stadium                                12.666667
Himachal Pradesh Cricket Association Stadium           12.000000
Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium    11.727273
Wankhede Stadium                                       11.526316
De Beers Diamond Oval                                  11.333333
Maharashtra Cricket Association Stadium                11.266667
JSCA International Stadium Complex                     10.857143
Sardar Patel Stadium, Motera                           10.833333
Name: venue, dtype: float64

matches
     
id	season	city	date	team1	team2	toss_winner	toss_decision	result	dl_applied	winner	win_by_runs	win_by_wickets	player_of_match	venue	umpire1	umpire2	umpire3
0	1	2017	Hyderabad	2017-04-05	Sunrisers Hyderabad	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Sunrisers Hyderabad	35	0	Yuvraj Singh	Rajiv Gandhi International Stadium, Uppal	AY Dandekar	NJ Llong	NaN
1	2	2017	Pune	2017-04-06	Mumbai Indians	Rising Pune Supergiant	Rising Pune Supergiant	field	normal	0	Rising Pune Supergiant	0	7	SPD Smith	Maharashtra Cricket Association Stadium	A Nand Kishore	S Ravi	NaN
2	3	2017	Rajkot	2017-04-07	Gujarat Lions	Kolkata Knight Riders	Kolkata Knight Riders	field	normal	0	Kolkata Knight Riders	0	10	CA Lynn	Saurashtra Cricket Association Stadium	Nitin Menon	CK Nandan	NaN
3	4	2017	Indore	2017-04-08	Rising Pune Supergiant	Kings XI Punjab	Kings XI Punjab	field	normal	0	Kings XI Punjab	0	6	GJ Maxwell	Holkar Cricket Stadium	AK Chaudhary	C Shamshuddin	NaN
4	5	2017	Bangalore	2017-04-08	Royal Challengers Bangalore	Delhi Daredevils	Royal Challengers Bangalore	bat	normal	0	Royal Challengers Bangalore	15	0	KM Jadhav	M Chinnaswamy Stadium	NaN	NaN	NaN
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
631	632	2016	Raipur	2016-05-22	Delhi Daredevils	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Royal Challengers Bangalore	0	6	V Kohli	Shaheed Veer Narayan Singh International Stadium	A Nand Kishore	BNJ Oxenford	NaN
632	633	2016	Bangalore	2016-05-24	Gujarat Lions	Royal Challengers Bangalore	Royal Challengers Bangalore	field	normal	0	Royal Challengers Bangalore	0	4	AB de Villiers	M Chinnaswamy Stadium	AK Chaudhary	HDPK Dharmasena	NaN
633	634	2016	Delhi	2016-05-25	Sunrisers Hyderabad	Kolkata Knight Riders	Kolkata Knight Riders	field	normal	0	Sunrisers Hyderabad	22	0	MC Henriques	Feroz Shah Kotla	M Erasmus	C Shamshuddin	NaN
634	635	2016	Delhi	2016-05-27	Gujarat Lions	Sunrisers Hyderabad	Sunrisers Hyderabad	field	normal	0	Sunrisers Hyderabad	0	4	DA Warner	Feroz Shah Kotla	M Erasmus	CK Nandan	NaN
635	636	2016	Bangalore	2016-05-29	Sunrisers Hyderabad	Royal Challengers Bangalore	Sunrisers Hyderabad	bat	normal	0	Sunrisers Hyderabad	8	0	BCJ Cutting	M Chinnaswamy Stadium	HDPK Dharmasena	BNJ Oxenford	NaN
636 rows × 18 columns


temp_df.groupby(['season','batsman'])['batsman_runs'].sum().reset_index().sort_values('batsman_runs',ascending=False).drop_duplicates(subset=['season'],keep='first').sort_values('season')
     
season	batsman	batsman_runs
115	2008	SE Marsh	616
229	2009	ML Hayden	572
446	2010	SR Tendulkar	618
502	2011	CH Gayle	608
684	2012	CH Gayle	733
910	2013	MEK Hussey	733
1088	2014	RV Uthappa	660
1148	2015	DA Warner	562
1383	2016	V Kohli	973
1422	2017	DA Warner	641

temp_df.groupby(['season','batsman'])['batsman_runs'].sum().reset_index().sort_values('batsman_runs',ascending=False)
     
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-214-5a08c989d1a3> in <module>
----> 1 temp_df.groupby(['season','batsman'])['batsman_runs'].sum().reset_index().sort_values('batsman_runs',ascending=False).first('season')

/usr/local/lib/python3.8/dist-packages/pandas/core/generic.py in first(self, offset)
   8193         """ """
   8194         if not isinstance(self.index, DatetimeIndex):
-> 8195             raise TypeError("'first' only supports a DatetimeIndex index")
   8196 
   8197         if len(self.index) == 0:

TypeError: 'first' only supports a DatetimeIndex index


     
#################################################################################




import numpy as np
import pandas as pd
     
Series is 1D and DataFrames are 2D objects
But why?
And what exactly is index?

# can we have multiple index? Let's try
index_val = [('cse',2019),('cse',2020),('cse',2021),('cse',2022),('ece',2019),('ece',2020),('ece',2021),('ece',2022)]
a = pd.Series([1,2,3,4,5,6,7,8],index=index_val)
a
     
(cse, 2019)    1
(cse, 2020)    2
(cse, 2021)    3
(cse, 2022)    4
(ece, 2019)    5
(ece, 2020)    6
(ece, 2021)    7
(ece, 2022)    8
dtype: int64

# The problem?
a['cse']
     
---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3360             try:
-> 3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

KeyError: 'cse'

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
<ipython-input-126-be5a0fa56305> in <module>
      1 # The problem?
----> 2 a['cse']

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in __getitem__(self, key)
    940 
    941         elif key_is_scalar:
--> 942             return self._get_value(key)
    943 
    944         if is_hashable(key):

/usr/local/lib/python3.8/dist-packages/pandas/core/series.py in _get_value(self, label, takeable)
   1049 
   1050         # Similar to Index.get_value, but we do not fall back to positional
-> 1051         loc = self.index.get_loc(label)
   1052         return self.index._get_values_for_loc(self, loc, label)
   1053 

/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:
-> 3363                 raise KeyError(key) from err
   3364 
   3365         if is_scalar(key) and isna(key) and not self.hasnans:

KeyError: 'cse'

# The solution -> multiindex series(also known as Hierarchical Indexing)
# multiple index levels within a single index
     

# how to create multiindex object
# 1. pd.MultiIndex.from_tuples()
index_val = [('cse',2019),('cse',2020),('cse',2021),('cse',2022),('ece',2019),('ece',2020),('ece',2021),('ece',2022)]
multiindex = pd.MultiIndex.from_tuples(index_val)
multiindex.levels[1]
# 2. pd.MultiIndex.from_product()
pd.MultiIndex.from_product([['cse','ece'],[2019,2020,2021,2022]])
     
MultiIndex([('cse', 2019),
            ('cse', 2020),
            ('cse', 2021),
            ('cse', 2022),
            ('ece', 2019),
            ('ece', 2020),
            ('ece', 2021),
            ('ece', 2022)],
           )

# level inside multiindex object
     

# creating a series with multiindex object
s = pd.Series([1,2,3,4,5,6,7,8],index=multiindex)
s
     
cse  2019    1
     2020    2
     2021    3
     2022    4
ece  2019    5
     2020    6
     2021    7
     2022    8
dtype: int64

# how to fetch items from such a series
s['cse']
     
2019    1
2020    2
2021    3
2022    4
dtype: int64

# a logical question to ask
     

# unstack
temp = s.unstack()
temp
     
2019	2020	2021	2022
cse	1	2	3	4
ece	5	6	7	8

# stack
temp.stack()
     
cse  2019    1
     2020    2
     2021    3
     2022    4
ece  2019    5
     2020    6
     2021    7
     2022    8
dtype: int64

# Then what was the point of multiindex series?
     

# multiindex dataframe
     

branch_df1 = pd.DataFrame(
    [
        [1,2],
        [3,4],
        [5,6],
        [7,8],
        [9,10],
        [11,12],
        [13,14],
        [15,16],
    ],
    index = multiindex,
    columns = ['avg_package','students']
)

branch_df1
     
avg_package	students
cse	2019	1	2
2020	3	4
2021	5	6
2022	7	8
ece	2019	9	10
2020	11	12
2021	13	14
2022	15	16

branch_df1['students']
     
cse  2019     2
     2020     4
     2021     6
     2022     8
ece  2019    10
     2020    12
     2021    14
     2022    16
Name: students, dtype: int64

# Are columns really different from index?
     

# multiindex df from columns perspective
branch_df2 = pd.DataFrame(
    [
        [1,2,0,0],
        [3,4,0,0],
        [5,6,0,0],
        [7,8,0,0],
    ],
    index = [2019,2020,2021,2022],
    columns = pd.MultiIndex.from_product([['delhi','mumbai'],['avg_package','students']])
)

branch_df2
     
delhi	mumbai
avg_package	students	avg_package	students
2019	1	2	0	0
2020	3	4	0	0
2021	5	6	0	0
2022	7	8	0	0

branch_df2.loc[2019]
     
delhi   avg_package    1
        students       2
mumbai  avg_package    0
        students       0
Name: 2019, dtype: int64

# Multiindex df in terms of both cols and index

branch_df3 = pd.DataFrame(
    [
        [1,2,0,0],
        [3,4,0,0],
        [5,6,0,0],
        [7,8,0,0],
        [9,10,0,0],
        [11,12,0,0],
        [13,14,0,0],
        [15,16,0,0],
    ],
    index = multiindex,
    columns = pd.MultiIndex.from_product([['delhi','mumbai'],['avg_package','students']])
)

branch_df3
     
delhi	mumbai
avg_package	students	avg_package	students
cse	2019	1	2	0	0
2020	3	4	0	0
2021	5	6	0	0
2022	7	8	0	0
ece	2019	9	10	0	0
2020	11	12	0	0
2021	13	14	0	0
2022	15	16	0	0
Stacking and Unstacking

branch_df3.stack().stack()
     
cse  2019  avg_package  delhi      1
                        mumbai     0
           students     delhi      2
                        mumbai     0
     2020  avg_package  delhi      3
                        mumbai     0
           students     delhi      4
                        mumbai     0
     2021  avg_package  delhi      5
                        mumbai     0
           students     delhi      6
                        mumbai     0
     2022  avg_package  delhi      7
                        mumbai     0
           students     delhi      8
                        mumbai     0
ece  2019  avg_package  delhi      9
                        mumbai     0
           students     delhi     10
                        mumbai     0
     2020  avg_package  delhi     11
                        mumbai     0
           students     delhi     12
                        mumbai     0
     2021  avg_package  delhi     13
                        mumbai     0
           students     delhi     14
                        mumbai     0
     2022  avg_package  delhi     15
                        mumbai     0
           students     delhi     16
                        mumbai     0
dtype: int64
Working with multiindex dataframes

# head and tail
branch_df3.head()
# shape
branch_df3.shape
# info
branch_df3.info()
# duplicated -> isnull
branch_df3.duplicated()
branch_df3.isnull()
     
<class 'pandas.core.frame.DataFrame'>
MultiIndex: 8 entries, ('cse', 2019) to ('ece', 2022)
Data columns (total 4 columns):
 #   Column                 Non-Null Count  Dtype
---  ------                 --------------  -----
 0   (delhi, avg_package)   8 non-null      int64
 1   (delhi, students)      8 non-null      int64
 2   (mumbai, avg_package)  8 non-null      int64
 3   (mumbai, students)     8 non-null      int64
dtypes: int64(4)
memory usage: 932.0+ bytes
delhi	mumbai
avg_package	students	avg_package	students
cse	2019	False	False	False	False
2020	False	False	False	False
2021	False	False	False	False
2022	False	False	False	False
ece	2019	False	False	False	False
2020	False	False	False	False
2021	False	False	False	False
2022	False	False	False	False

# Extracting rows single
branch_df3.loc[('cse',2022)]
     
delhi   avg_package    7
        students       8
mumbai  avg_package    0
        students       0
Name: (cse, 2022), dtype: int64

# multiple
branch_df3.loc[('cse',2019):('ece',2020):2]
     
delhi	mumbai
avg_package	students	avg_package	students
cse	2019	1	2	0	0
2021	5	6	0	0
ece	2019	9	10	0	0

# using iloc
branch_df3.iloc[0:5:2]
     
delhi	mumbai
avg_package	students	avg_package	students
cse	2019	1	2	0	0
2021	5	6	0	0
ece	2019	9	10	0	0

# Extracting cols
branch_df3['delhi']['students']
     
cse  2019     2
     2020     4
     2021     6
     2022     8
ece  2019    10
     2020    12
     2021    14
     2022    16
Name: students, dtype: int64

branch_df3.iloc[:,1:3]
     
delhi	mumbai
students	avg_package
cse	2019	2	0
2020	4	0
2021	6	0
2022	8	0
ece	2019	10	0
2020	12	0
2021	14	0
2022	16	0

# Extracting both
branch_df3.iloc[[0,4],[1,2]]
     
delhi	mumbai
students	avg_package
cse	2019	2	0
ece	2019	10	0

# sort index
# both -> descending -> diff order
# based on one level
branch_df3.sort_index(ascending=False)
branch_df3.sort_index(ascending=[False,True])
branch_df3.sort_index(level=0,ascending=[False])
     
delhi	mumbai
avg_package	students	avg_package	students
ece	2019	9	10	0	0
2020	11	12	0	0
2021	13	14	0	0
2022	15	16	0	0
cse	2019	1	2	0	0
2020	3	4	0	0
2021	5	6	0	0
2022	7	8	0	0

# multiindex dataframe(col) -> transpose
branch_df3.transpose()
     
cse	ece
2019	2020	2021	2022	2019	2020	2021	2022
delhi	avg_package	1	3	5	7	9	11	13	15
students	2	4	6	8	10	12	14	16
mumbai	avg_package	0	0	0	0	0	0	0	0
students	0	0	0	0	0	0	0	0

# swaplevel
branch_df3.swaplevel(axis=1)
     
avg_package	students	avg_package	students
delhi	delhi	mumbai	mumbai
cse	2019	1	2	0	0
2020	3	4	0	0
2021	5	6	0	0
2022	7	8	0	0
ece	2019	9	10	0	0
2020	11	12	0	0
2021	13	14	0	0
2022	15	16	0	0
Long Vs Wide Data
image.png

Wide format is where we have a single row for every data point with multiple columns to hold the values of various attributes.

Long format is where, for each data point we have as many rows as the number of attributes and each row contains the value of a particular attribute for a given data point.


# melt -> simple example branch
# wide to long
pd.DataFrame({'cse':[120]}).melt()
     
variable	value
0	cse	120

# melt -> branch with year
pd.DataFrame({'cse':[120],'ece':[100],'mech':[50]}).melt(var_name='branch',value_name='num_students')
     
branch	num_students
0	cse	120
1	ece	100
2	mech	50

pd.DataFrame(
    {
        'branch':['cse','ece','mech'],
        '2020':[100,150,60],
        '2021':[120,130,80],
        '2022':[150,140,70]
    }
).melt(id_vars=['branch'],var_name='year',value_name='students')
     
branch	year	students
0	cse	2020	100
1	ece	2020	150
2	mech	2020	60
3	cse	2021	120
4	ece	2021	130
5	mech	2021	80
6	cse	2022	150
7	ece	2022	140
8	mech	2022	70

# melt -> real world example
death = pd.read_csv('/content/time_series_covid19_deaths_global.csv')
confirm = pd.read_csv('/content/time_series_covid19_confirmed_global.csv')
     

death.head()
     
Province/State	Country/Region	Lat	Long	1/22/20	1/23/20	1/24/20	1/25/20	1/26/20	1/27/20	...	12/24/22	12/25/22	12/26/22	12/27/22	12/28/22	12/29/22	12/30/22	12/31/22	1/1/23	1/2/23
0	NaN	Afghanistan	33.93911	67.709953	0	0	0	0	0	0	...	7845	7846	7846	7846	7846	7847	7847	7849	7849	7849
1	NaN	Albania	41.15330	20.168300	0	0	0	0	0	0	...	3595	3595	3595	3595	3595	3595	3595	3595	3595	3595
2	NaN	Algeria	28.03390	1.659600	0	0	0	0	0	0	...	6881	6881	6881	6881	6881	6881	6881	6881	6881	6881
3	NaN	Andorra	42.50630	1.521800	0	0	0	0	0	0	...	165	165	165	165	165	165	165	165	165	165
4	NaN	Angola	-11.20270	17.873900	0	0	0	0	0	0	...	1928	1928	1928	1930	1930	1930	1930	1930	1930	1930
5 rows × 1081 columns


confirm.head()
     
Province/State	Country/Region	Lat	Long	1/22/20	1/23/20	1/24/20	1/25/20	1/26/20	1/27/20	...	12/24/22	12/25/22	12/26/22	12/27/22	12/28/22	12/29/22	12/30/22	12/31/22	1/1/23	1/2/23
0	NaN	Afghanistan	33.93911	67.709953	0	0	0	0	0	0	...	207310	207399	207438	207460	207493	207511	207550	207559	207616	207627
1	NaN	Albania	41.15330	20.168300	0	0	0	0	0	0	...	333749	333749	333751	333751	333776	333776	333806	333806	333811	333812
2	NaN	Algeria	28.03390	1.659600	0	0	0	0	0	0	...	271194	271198	271198	271202	271208	271217	271223	271228	271229	271229
3	NaN	Andorra	42.50630	1.521800	0	0	0	0	0	0	...	47686	47686	47686	47686	47751	47751	47751	47751	47751	47751
4	NaN	Angola	-11.20270	17.873900	0	0	0	0	0	0	...	104973	104973	104973	105095	105095	105095	105095	105095	105095	105095
5 rows × 1081 columns


death = death.melt(id_vars=['Province/State','Country/Region','Lat','Long'],var_name='date',value_name='num_deaths')
confirm = confirm.melt(id_vars=['Province/State','Country/Region','Lat','Long'],var_name='date',value_name='num_cases')
     

death.head()
     
Province/State	Country/Region	Lat	Long	date	num_deaths
0	NaN	Afghanistan	33.93911	67.709953	1/22/20	0
1	NaN	Albania	41.15330	20.168300	1/22/20	0
2	NaN	Algeria	28.03390	1.659600	1/22/20	0
3	NaN	Andorra	42.50630	1.521800	1/22/20	0
4	NaN	Angola	-11.20270	17.873900	1/22/20	0

confirm.merge(death,on=['Province/State','Country/Region','Lat','Long','date'])[['Country/Region','date','num_cases','num_deaths']]
     
Country/Region	date	num_cases	num_deaths
0	Afghanistan	1/22/20	0	0
1	Albania	1/22/20	0	0
2	Algeria	1/22/20	0	0
3	Andorra	1/22/20	0	0
4	Angola	1/22/20	0	0
...	...	...	...	...
311248	West Bank and Gaza	1/2/23	703228	5708
311249	Winter Olympics 2022	1/2/23	535	0
311250	Yemen	1/2/23	11945	2159
311251	Zambia	1/2/23	334661	4024
311252	Zimbabwe	1/2/23	259981	5637
311253 rows × 4 columns



     


     
Pivot Table
The pivot table takes simple column-wise data as input, and groups the entries into a two-dimensional table that provides a multidimensional summarization of the data.


import numpy as np
import pandas as pd
import seaborn as sns
     

df = sns.load_dataset('tips')
df.head()
     
total_bill	tip	sex	smoker	day	time	size
0	16.99	1.01	Female	No	Sun	Dinner	2
1	10.34	1.66	Male	No	Sun	Dinner	3
2	21.01	3.50	Male	No	Sun	Dinner	3
3	23.68	3.31	Male	No	Sun	Dinner	2
4	24.59	3.61	Female	No	Sun	Dinner	4

df.groupby('sex')[['total_bill']].mean()
     
total_bill
sex	
Male	20.744076
Female	18.056897

df.groupby(['sex','smoker'])[['total_bill']].mean().unstack()
     
total_bill
smoker	Yes	No
sex		
Male	22.284500	19.791237
Female	17.977879	18.105185

df.pivot_table(index='sex',columns='smoker',values='total_bill')
     
smoker	Yes	No
sex		
Male	22.284500	19.791237
Female	17.977879	18.105185


     


     

# aggfunc
df.pivot_table(index='sex',columns='smoker',values='total_bill',aggfunc='std')
     
smoker	Yes	No
sex		
Male	9.911845	8.726566
Female	9.189751	7.286455

# all cols together
df.pivot_table(index='sex',columns='smoker')['size']
     
smoker	Yes	No
sex		
Male	2.500000	2.711340
Female	2.242424	2.592593

# multidimensional
df.pivot_table(index=['sex','smoker'],columns=['day','time'],aggfunc={'size':'mean','tip':'max','total_bill':'sum'},margins=True)
     
size	tip	total_bill
day	Thur	Fri	Sat	Sun	All	Thur	Fri	...	All	Thur	Fri	Sat	Sun	All
time	Lunch	Dinner	Lunch	Dinner	Dinner	Dinner		Lunch	Dinner	Lunch	...		Lunch	Dinner	Lunch	Dinner	Lunch	Dinner	Lunch	Dinner	
sex	smoker																					
Male	Yes	2.300000	NaN	1.666667	2.400000	2.629630	2.600000	2.500000	5.00	NaN	2.20	...	10.0	191.71	0.00	34.16	129.46	0.0	589.62	0.0	392.12	1337.07
No	2.500000	NaN	NaN	2.000000	2.656250	2.883721	2.711340	6.70	NaN	NaN	...	9.0	369.73	0.00	0.00	34.95	0.0	637.73	0.0	877.34	1919.75
Female	Yes	2.428571	NaN	2.000000	2.000000	2.200000	2.500000	2.242424	5.00	NaN	3.48	...	6.5	134.53	0.00	39.78	48.80	0.0	304.00	0.0	66.16	593.27
No	2.500000	2.0	3.000000	2.000000	2.307692	3.071429	2.592593	5.17	3.0	3.00	...	5.2	381.58	18.78	15.98	22.75	0.0	247.05	0.0	291.54	977.68
All		2.459016	2.0	2.000000	2.166667	2.517241	2.842105	2.569672	6.70	3.0	3.48	...	10.0	1077.55	18.78	89.92	235.96	NaN	1778.40	NaN	1627.16	4827.77
5 rows × 23 columns


# margins
df.pivot_table(index='sex',columns='smoker',values='total_bill',aggfunc='sum',margins=True)
     
smoker	Yes	No	All
sex			
Male	1337.07	1919.75	3256.82
Female	593.27	977.68	1570.95
All	1930.34	2897.43	4827.77

# plotting graphs
df = pd.read_csv('/content/expense_data.csv')
     

df.head()
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1
0	3/2/2022 10:11	CUB - online payment	Food	NaN	Brownie	50.0	Expense	NaN	50.0	INR	50.0
1	3/2/2022 10:11	CUB - online payment	Other	NaN	To lended people	300.0	Expense	NaN	300.0	INR	300.0
2	3/1/2022 19:50	CUB - online payment	Food	NaN	Dinner	78.0	Expense	NaN	78.0	INR	78.0
3	3/1/2022 18:56	CUB - online payment	Transportation	NaN	Metro	30.0	Expense	NaN	30.0	INR	30.0
4	3/1/2022 18:22	CUB - online payment	Food	NaN	Snacks	67.0	Expense	NaN	67.0	INR	67.0

df['Category'].value_counts()
     
Food                156
Other                60
Transportation       31
Apparel               7
Household             6
Allowance             6
Social Life           5
Education             1
Salary                1
Self-development      1
Beauty                1
Gift                  1
Petty cash            1
Name: Category, dtype: int64

df.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 277 entries, 0 to 276
Data columns (total 11 columns):
 #   Column          Non-Null Count  Dtype  
---  ------          --------------  -----  
 0   Date            277 non-null    object 
 1   Account         277 non-null    object 
 2   Category        277 non-null    object 
 3   Subcategory     0 non-null      float64
 4   Note            273 non-null    object 
 5   INR             277 non-null    float64
 6   Income/Expense  277 non-null    object 
 7   Note.1          0 non-null      float64
 8   Amount          277 non-null    float64
 9   Currency        277 non-null    object 
 10  Account.1       277 non-null    float64
dtypes: float64(5), object(6)
memory usage: 23.9+ KB

df['Date'] = pd.to_datetime(df['Date'])
     

df.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 277 entries, 0 to 276
Data columns (total 11 columns):
 #   Column          Non-Null Count  Dtype         
---  ------          --------------  -----         
 0   Date            277 non-null    datetime64[ns]
 1   Account         277 non-null    object        
 2   Category        277 non-null    object        
 3   Subcategory     0 non-null      float64       
 4   Note            273 non-null    object        
 5   INR             277 non-null    float64       
 6   Income/Expense  277 non-null    object        
 7   Note.1          0 non-null      float64       
 8   Amount          277 non-null    float64       
 9   Currency        277 non-null    object        
 10  Account.1       277 non-null    float64       
dtypes: datetime64[ns](1), float64(5), object(5)
memory usage: 23.9+ KB

df['month'] = df['Date'].dt.month_name()
     

df.head()
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1	month
0	2022-03-02 10:11:00	CUB - online payment	Food	NaN	Brownie	50.0	Expense	NaN	50.0	INR	50.0	March
1	2022-03-02 10:11:00	CUB - online payment	Other	NaN	To lended people	300.0	Expense	NaN	300.0	INR	300.0	March
2	2022-03-01 19:50:00	CUB - online payment	Food	NaN	Dinner	78.0	Expense	NaN	78.0	INR	78.0	March
3	2022-03-01 18:56:00	CUB - online payment	Transportation	NaN	Metro	30.0	Expense	NaN	30.0	INR	30.0	March
4	2022-03-01 18:22:00	CUB - online payment	Food	NaN	Snacks	67.0	Expense	NaN	67.0	INR	67.0	March

df.pivot_table(index='month',columns='Category',values='INR',aggfunc='sum',fill_value=0).plot()
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f8976423df0>


df.pivot_table(index='month',columns='Income/Expense',values='INR',aggfunc='sum',fill_value=0).plot()
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f8976118f70>


df.pivot_table(index='month',columns='Account',values='INR',aggfunc='sum',fill_value=0).plot()
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f89760dbe50>



     
###################################################################################




import numpy as np
import pandas as pd
     
Timestamp Object
Time stamps reference particular moments in time (e.g., Oct 24th, 2022 at 7:00pm)

Creating Timestamp objects

# creating a timestamp
type(pd.Timestamp('2023/1/5'))
     
pandas._libs.tslibs.timestamps.Timestamp

# variations
pd.Timestamp('2023-1-5')
pd.Timestamp('2023, 1, 5')
     
Timestamp('2023-01-05 00:00:00')

# only year
pd.Timestamp('2023')
     
Timestamp('2023-01-01 00:00:00')

# using text
pd.Timestamp('5th January 2023')
     
Timestamp('2023-01-05 00:00:00')

# providing time also
pd.Timestamp('5th January 2023 9:21AM')
# pd.Timestamp('2023/1/5/9/21')
     
---------------------------------------------------------------------------
ParserError                               Traceback (most recent call last)
/usr/local/lib/python3.8/dist-packages/pandas/_libs/tslibs/conversion.pyx in pandas._libs.tslibs.conversion._convert_str_to_tsobject()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/tslibs/parsing.pyx in pandas._libs.tslibs.parsing.parse_datetime_string()

/usr/local/lib/python3.8/dist-packages/dateutil/parser/_parser.py in parse(timestr, parserinfo, **kwargs)
   1367     else:
-> 1368         return DEFAULTPARSER.parse(timestr, **kwargs)
   1369 

/usr/local/lib/python3.8/dist-packages/dateutil/parser/_parser.py in parse(self, timestr, default, ignoretz, tzinfos, **kwargs)
    642         if res is None:
--> 643             raise ParserError("Unknown string format: %s", timestr)
    644 

ParserError: Unknown string format: 2023/1/5/9/21

During handling of the above exception, another exception occurred:

ValueError                                Traceback (most recent call last)
<ipython-input-16-d14ab29f95a1> in <module>
      1 # providing time also
      2 pd.Timestamp('5th January 2023 9:21AM')
----> 3 pd.Timestamp('2023/1/5/9/21')

/usr/local/lib/python3.8/dist-packages/pandas/_libs/tslibs/timestamps.pyx in pandas._libs.tslibs.timestamps.Timestamp.__new__()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/tslibs/conversion.pyx in pandas._libs.tslibs.conversion.convert_to_tsobject()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/tslibs/conversion.pyx in pandas._libs.tslibs.conversion._convert_str_to_tsobject()

ValueError: could not convert string to Timestamp

# AM and PM
     

# using datetime.datetime object
import datetime as dt

x = pd.Timestamp(dt.datetime(2023,1,5,9,21,56))
x
     
Timestamp('2023-01-05 09:21:56')

# fetching attributes
x.year
x.month
x.day
x.hour
x.minute
x.second
     
56

# why separate objects to handle data and time when python already has datetime functionality?
     
syntax wise datetime is very convenient
But the performance takes a hit while working with huge data. List vs Numpy Array
The weaknesses of Python's datetime format inspired the NumPy team to add a set of native time series data type to NumPy.
The datetime64 dtype encodes dates as 64-bit integers, and thus allows arrays of dates to be represented very compactly.

import numpy as np
date = np.array('2015-07-04', dtype=np.datetime64)
date
     
array('2015-07-04', dtype='datetime64[D]')

date + np.arange(12)
     
array(['2015-07-04', '2015-07-05', '2015-07-06', '2015-07-07',
       '2015-07-08', '2015-07-09', '2015-07-10', '2015-07-11',
       '2015-07-12', '2015-07-13', '2015-07-14', '2015-07-15'],
      dtype='datetime64[D]')
Because of the uniform type in NumPy datetime64 arrays, this type of operation can be accomplished much more quickly than if we were working directly with Python's datetime objects, especially as arrays get large

Pandas Timestamp object combines the ease-of-use of python datetime with the efficient storage and vectorized interface of numpy.datetime64

From a group of these Timestamp objects, Pandas can construct a DatetimeIndex that can be used to index data in a Series or DataFrame

DatetimeIndex Object
A collection of pandas timestamp


# from strings
type(pd.DatetimeIndex(['2023/1/1','2022/1/1','2021/1/1']))
     
pandas.core.indexes.datetimes.DatetimeIndex

# using python datetime object
pd.DatetimeIndex([dt.datetime(2023,1,1),dt.datetime(2022,1,1),dt.datetime(2021,1,1)])
     
DatetimeIndex(['2023-01-01', '2022-01-01', '2021-01-01'], dtype='datetime64[ns]', freq=None)

# using pd.timestamps
dt_index = pd.DatetimeIndex([pd.Timestamp(2023,1,1),pd.Timestamp(2022,1,1),pd.Timestamp(2021,1,1)])
     

# using datatimeindex as series index

pd.Series([1,2,3],index=dt_index)
     
2023-01-01    1
2022-01-01    2
2021-01-01    3
dtype: int64
date_range function

# generate daily dates in a given range
pd.date_range(start='2023/1/5',end='2023/2/28',freq='3D')
     
DatetimeIndex(['2023-01-05', '2023-01-08', '2023-01-11', '2023-01-14',
               '2023-01-17', '2023-01-20', '2023-01-23', '2023-01-26',
               '2023-01-29', '2023-02-01', '2023-02-04', '2023-02-07',
               '2023-02-10', '2023-02-13', '2023-02-16', '2023-02-19',
               '2023-02-22', '2023-02-25', '2023-02-28'],
              dtype='datetime64[ns]', freq='3D')

# alternate days in a given range
pd.date_range(start='2023/1/5',end='2023/2/28',freq='3D')
     

# B -> business days
pd.date_range(start='2023/1/5',end='2023/2/28',freq='B')
     
DatetimeIndex(['2023-01-05', '2023-01-06', '2023-01-09', '2023-01-10',
               '2023-01-11', '2023-01-12', '2023-01-13', '2023-01-16',
               '2023-01-17', '2023-01-18', '2023-01-19', '2023-01-20',
               '2023-01-23', '2023-01-24', '2023-01-25', '2023-01-26',
               '2023-01-27', '2023-01-30', '2023-01-31', '2023-02-01',
               '2023-02-02', '2023-02-03', '2023-02-06', '2023-02-07',
               '2023-02-08', '2023-02-09', '2023-02-10', '2023-02-13',
               '2023-02-14', '2023-02-15', '2023-02-16', '2023-02-17',
               '2023-02-20', '2023-02-21', '2023-02-22', '2023-02-23',
               '2023-02-24', '2023-02-27', '2023-02-28'],
              dtype='datetime64[ns]', freq='B')

# W -> one week per day
pd.date_range(start='2023/1/5',end='2023/2/28',freq='W-THU')
     
DatetimeIndex(['2023-01-05', '2023-01-12', '2023-01-19', '2023-01-26',
               '2023-02-02', '2023-02-09', '2023-02-16', '2023-02-23'],
              dtype='datetime64[ns]', freq='W-THU')

# H -> Hourly data(factor)
pd.date_range(start='2023/1/5',end='2023/2/28',freq='6H')
     
DatetimeIndex(['2023-01-05 00:00:00', '2023-01-05 06:00:00',
               '2023-01-05 12:00:00', '2023-01-05 18:00:00',
               '2023-01-06 00:00:00', '2023-01-06 06:00:00',
               '2023-01-06 12:00:00', '2023-01-06 18:00:00',
               '2023-01-07 00:00:00', '2023-01-07 06:00:00',
               ...
               '2023-02-25 18:00:00', '2023-02-26 00:00:00',
               '2023-02-26 06:00:00', '2023-02-26 12:00:00',
               '2023-02-26 18:00:00', '2023-02-27 00:00:00',
               '2023-02-27 06:00:00', '2023-02-27 12:00:00',
               '2023-02-27 18:00:00', '2023-02-28 00:00:00'],
              dtype='datetime64[ns]', length=217, freq='6H')

# M -> Month end
pd.date_range(start='2023/1/5',end='2023/2/28',freq='M')
     
DatetimeIndex(['2023-01-31', '2023-02-28'], dtype='datetime64[ns]', freq='M')

# MS -> Month start
pd.date_range(start='2023/1/5',end='2023/2/28',freq='MS')
     
DatetimeIndex(['2023-02-01'], dtype='datetime64[ns]', freq='MS')

# A -> Year end
pd.date_range(start='2023/1/5',end='2030/2/28',freq='A')
     
DatetimeIndex(['2023-12-31', '2024-12-31', '2025-12-31', '2026-12-31',
               '2027-12-31', '2028-12-31', '2029-12-31'],
              dtype='datetime64[ns]', freq='A-DEC')

# using periods(number of results)
pd.date_range(start='2023/1/5',periods=25,freq='M')
     
DatetimeIndex(['2023-01-31', '2023-02-28', '2023-03-31', '2023-04-30',
               '2023-05-31', '2023-06-30', '2023-07-31', '2023-08-31',
               '2023-09-30', '2023-10-31', '2023-11-30', '2023-12-31',
               '2024-01-31', '2024-02-29', '2024-03-31', '2024-04-30',
               '2024-05-31', '2024-06-30', '2024-07-31', '2024-08-31',
               '2024-09-30', '2024-10-31', '2024-11-30', '2024-12-31',
               '2025-01-31'],
              dtype='datetime64[ns]', freq='M')
to_datetime function
converts an existing objects to pandas timestamp/datetimeindex object


# simple series example

s = pd.Series(['2023/1/1','2022/1/1','2021/1/1'])
pd.to_datetime(s).dt.day_name()
     
0      Sunday
1    Saturday
2      Friday
dtype: object

# with errors
s = pd.Series(['2023/1/1','2022/1/1','2021/130/1'])
pd.to_datetime(s,errors='coerce').dt.month_name()
     
0    January
1    January
2        NaN
dtype: object

df = pd.read_csv('/content/expense_data.csv')
df.shape
     
(277, 11)

df.head()
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1
0	3/2/2022 10:11	CUB - online payment	Food	NaN	Brownie	50.0	Expense	NaN	50.0	INR	50.0
1	3/2/2022 10:11	CUB - online payment	Other	NaN	To lended people	300.0	Expense	NaN	300.0	INR	300.0
2	3/1/2022 19:50	CUB - online payment	Food	NaN	Dinner	78.0	Expense	NaN	78.0	INR	78.0
3	3/1/2022 18:56	CUB - online payment	Transportation	NaN	Metro	30.0	Expense	NaN	30.0	INR	30.0
4	3/1/2022 18:22	CUB - online payment	Food	NaN	Snacks	67.0	Expense	NaN	67.0	INR	67.0

df['Date'] = pd.to_datetime(df['Date'])
     

df.info()
     
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 277 entries, 0 to 276
Data columns (total 11 columns):
 #   Column          Non-Null Count  Dtype         
---  ------          --------------  -----         
 0   Date            277 non-null    datetime64[ns]
 1   Account         277 non-null    object        
 2   Category        277 non-null    object        
 3   Subcategory     0 non-null      float64       
 4   Note            273 non-null    object        
 5   INR             277 non-null    float64       
 6   Income/Expense  277 non-null    object        
 7   Note.1          0 non-null      float64       
 8   Amount          277 non-null    float64       
 9   Currency        277 non-null    object        
 10  Account.1       277 non-null    float64       
dtypes: datetime64[ns](1), float64(5), object(5)
memory usage: 23.9+ KB
dt accessor
Accessor object for datetimelike properties of the Series values.


df['Date'].dt.is_quarter_start
     
0      False
1      False
2      False
3      False
4      False
       ...  
272    False
273    False
274    False
275    False
276    False
Name: Date, Length: 277, dtype: bool

# plot graph
import matplotlib.pyplot as plt
plt.plot(df['Date'],df['INR'])
     
[<matplotlib.lines.Line2D at 0x7f89b2206880>]


# day name wise bar chart/month wise bar chart

df['day_name'] = df['Date'].dt.day_name()
     

df.head()
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1	day_name
0	2022-03-02 10:11:00	CUB - online payment	Food	NaN	Brownie	50.0	Expense	NaN	50.0	INR	50.0	Wednesday
1	2022-03-02 10:11:00	CUB - online payment	Other	NaN	To lended people	300.0	Expense	NaN	300.0	INR	300.0	Wednesday
2	2022-03-01 19:50:00	CUB - online payment	Food	NaN	Dinner	78.0	Expense	NaN	78.0	INR	78.0	Tuesday
3	2022-03-01 18:56:00	CUB - online payment	Transportation	NaN	Metro	30.0	Expense	NaN	30.0	INR	30.0	Tuesday
4	2022-03-01 18:22:00	CUB - online payment	Food	NaN	Snacks	67.0	Expense	NaN	67.0	INR	67.0	Tuesday

df.groupby('day_name')['INR'].mean().plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f89b18629a0>


df['month_name'] = df['Date'].dt.month_name()
     

df.head()
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1	day_name	month_name
0	2022-03-02 10:11:00	CUB - online payment	Food	NaN	Brownie	50.0	Expense	NaN	50.0	INR	50.0	Wednesday	March
1	2022-03-02 10:11:00	CUB - online payment	Other	NaN	To lended people	300.0	Expense	NaN	300.0	INR	300.0	Wednesday	March
2	2022-03-01 19:50:00	CUB - online payment	Food	NaN	Dinner	78.0	Expense	NaN	78.0	INR	78.0	Tuesday	March
3	2022-03-01 18:56:00	CUB - online payment	Transportation	NaN	Metro	30.0	Expense	NaN	30.0	INR	30.0	Tuesday	March
4	2022-03-01 18:22:00	CUB - online payment	Food	NaN	Snacks	67.0	Expense	NaN	67.0	INR	67.0	Tuesday	March

df.groupby('month_name')['INR'].sum().plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f89b1905c40>


df[df['Date'].dt.is_month_end]
     
Date	Account	Category	Subcategory	Note	INR	Income/Expense	Note.1	Amount	Currency	Account.1	day_name	month_name
7	2022-02-28 11:56:00	CUB - online payment	Food	NaN	Pizza	339.15	Expense	NaN	339.15	INR	339.15	Monday	February
8	2022-02-28 11:45:00	CUB - online payment	Other	NaN	From kumara	200.00	Income	NaN	200.00	INR	200.00	Monday	February
61	2022-01-31 08:44:00	CUB - online payment	Transportation	NaN	Vnr to apk	50.00	Expense	NaN	50.00	INR	50.00	Monday	January
62	2022-01-31 08:27:00	CUB - online payment	Other	NaN	To vicky	200.00	Expense	NaN	200.00	INR	200.00	Monday	January
63	2022-01-31 08:26:00	CUB - online payment	Transportation	NaN	To ksr station	153.00	Expense	NaN	153.00	INR	153.00	Monday	January
242	2021-11-30 14:24:00	CUB - online payment	Gift	NaN	Bharath birthday	115.00	Expense	NaN	115.00	INR	115.00	Tuesday	November
243	2021-11-30 14:17:00	CUB - online payment	Food	NaN	Lunch with company	128.00	Expense	NaN	128.00	INR	128.00	Tuesday	November
244	2021-11-30 10:11:00	CUB - online payment	Food	NaN	Breakfast	70.00	Expense	NaN	70.00	INR	70.00	Tuesday	November


   
####################################################################################



import pandas as pd
import numpy as np
     

# What are vectorized operations
a = np.array([1,2,3,4])
a * 4
     
array([ 4,  8, 12, 16])

# problem in vectorized opertions in vanilla python
s = ['cat','mat',None,'rat']

[i.startswith('c') for i in s]
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-19-3fe713c7ebb8> in <module>
      2 s = ['cat','mat',None,'rat']
      3 
----> 4 [i.startswith('c') for i in s]

<ipython-input-19-3fe713c7ebb8> in <listcomp>(.0)
      2 s = ['cat','mat',None,'rat']
      3 
----> 4 [i.startswith('c') for i in s]

AttributeError: 'NoneType' object has no attribute 'startswith'

# How pandas solves this issue?

s = pd.Series(['cat','mat',None,'rat'])
# string accessor
s.str.startswith('c')

# fast and optimized
     
0     True
1    False
2     None
3    False
dtype: object

# import titanic
df = pd.read_csv('/content/titanic.csv')
df['Name']
     
0                                Braund, Mr. Owen Harris
1      Cumings, Mrs. John Bradley (Florence Briggs Th...
2                                 Heikkinen, Miss. Laina
3           Futrelle, Mrs. Jacques Heath (Lily May Peel)
4                               Allen, Mr. William Henry
                             ...                        
886                                Montvila, Rev. Juozas
887                         Graham, Miss. Margaret Edith
888             Johnston, Miss. Catherine Helen "Carrie"
889                                Behr, Mr. Karl Howell
890                                  Dooley, Mr. Patrick
Name: Name, Length: 891, dtype: object

# Common Functions
# lower/upper/capitalize/title
df['Name'].str.upper()
df['Name'].str.capitalize()
df['Name'].str.title()
# len
df['Name'][df['Name'].str.len() == 82].values[0]
# strip
"                   nitish                              ".strip()
df['Name'].str.strip()
     
0                                Braund, Mr. Owen Harris
1      Cumings, Mrs. John Bradley (Florence Briggs Th...
2                                 Heikkinen, Miss. Laina
3           Futrelle, Mrs. Jacques Heath (Lily May Peel)
4                               Allen, Mr. William Henry
                             ...                        
886                                Montvila, Rev. Juozas
887                         Graham, Miss. Margaret Edith
888             Johnston, Miss. Catherine Helen "Carrie"
889                                Behr, Mr. Karl Howell
890                                  Dooley, Mr. Patrick
Name: Name, Length: 891, dtype: object

# split -> get
df['lastname'] = df['Name'].str.split(',').str.get(0)
df.head()
     
PassengerId	Survived	Pclass	Name	Sex	Age	SibSp	Parch	Ticket	Fare	Cabin	Embarked	lastname
0	1	0	3	Braund, Mr. Owen Harris	male	22.0	1	0	A/5 21171	7.2500	NaN	S	Braund
1	2	1	1	Cumings, Mrs. John Bradley (Florence Briggs Th...	female	38.0	1	0	PC 17599	71.2833	C85	C	Cumings
2	3	1	3	Heikkinen, Miss. Laina	female	26.0	0	0	STON/O2. 3101282	7.9250	NaN	S	Heikkinen
3	4	1	1	Futrelle, Mrs. Jacques Heath (Lily May Peel)	female	35.0	1	0	113803	53.1000	C123	S	Futrelle
4	5	0	3	Allen, Mr. William Henry	male	35.0	0	0	373450	8.0500	NaN	S	Allen

df[['title','firstname']] = df['Name'].str.split(',').str.get(1).str.strip().str.split(' ', n=1, expand=True)
df.head()

df['title'].value_counts()
     
Mr.          517
Miss.        182
Mrs.         125
Master.       40
Dr.            7
Rev.           6
Mlle.          2
Major.         2
Col.           2
the            1
Capt.          1
Ms.            1
Sir.           1
Lady.          1
Mme.           1
Don.           1
Jonkheer.      1
Name: title, dtype: int64

# replace
df['title'] = df['title'].str.replace('Ms.','Miss.')
df['title'] = df['title'].str.replace('Mlle.','Miss.')
     
<ipython-input-60-9403e5934d1f>:2: FutureWarning: The default value of regex will change from True to False in a future version.
  df['title'] = df['title'].str.replace('Ms.','Miss.')
<ipython-input-60-9403e5934d1f>:3: FutureWarning: The default value of regex will change from True to False in a future version.
  df['title'] = df['title'].str.replace('Mlle.','Miss.')

df['title'].value_counts()
     
Mr.          517
Miss.        185
Mrs.         125
Master.       40
Dr.            7
Rev.           6
Major.         2
Col.           2
Don.           1
Mme.           1
Lady.          1
Sir.           1
Capt.          1
the            1
Jonkheer.      1
Name: title, dtype: int64

# filtering
# startswith/endswith
df[df['firstname'].str.endswith('A')]
# isdigit/isalpha...
df[df['firstname'].str.isdigit()]
     
PassengerId	Survived	Pclass	Name	Sex	Age	SibSp	Parch	Ticket	Fare	Cabin	Embarked	lastname	title	firstname

# applying regex
# contains
# search john -> both case
df[df['firstname'].str.contains('john',case=False)]
# find lastnames with start and end char vowel
df[df['lastname'].str.contains('^[^aeiouAEIOU].+[^aeiouAEIOU]$')]
     
PassengerId	Survived	Pclass	Name	Sex	Age	SibSp	Parch	Ticket	Fare	Cabin	Embarked	lastname	title	firstname
0	1	0	3	Braund, Mr. Owen Harris	male	22.0	1	0	A/5 21171	7.2500	NaN	S	Braund	Mr.	Owen Harris
1	2	1	1	Cumings, Mrs. John Bradley (Florence Briggs Th...	female	38.0	1	0	PC 17599	71.2833	C85	C	Cumings	Mrs.	John Bradley (Florence Briggs Thayer)
2	3	1	3	Heikkinen, Miss. Laina	female	26.0	0	0	STON/O2. 3101282	7.9250	NaN	S	Heikkinen	Miss.	Laina
5	6	0	3	Moran, Mr. James	male	NaN	0	0	330877	8.4583	NaN	Q	Moran	Mr.	James
6	7	0	1	McCarthy, Mr. Timothy J	male	54.0	0	0	17463	51.8625	E46	S	McCarthy	Mr.	Timothy J
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
884	885	0	3	Sutehall, Mr. Henry Jr	male	25.0	0	0	SOTON/OQ 392076	7.0500	NaN	S	Sutehall	Mr.	Henry Jr
887	888	1	1	Graham, Miss. Margaret Edith	female	19.0	0	0	112053	30.0000	B42	S	Graham	Miss.	Margaret Edith
888	889	0	3	Johnston, Miss. Catherine Helen "Carrie"	female	NaN	1	2	W./C. 6607	23.4500	NaN	S	Johnston	Miss.	Catherine Helen "Carrie"
889	890	1	1	Behr, Mr. Karl Howell	male	26.0	0	0	111369	30.0000	C148	C	Behr	Mr.	Karl Howell
890	891	0	3	Dooley, Mr. Patrick	male	32.0	0	0	370376	7.7500	NaN	Q	Dooley	Mr.	Patrick
671 rows × 15 columns


# slicing
df['Name'].str[::-1]
     
0                                sirraH newO .rM ,dnuarB
1      )reyahT sggirB ecnerolF( yeldarB nhoJ .srM ,sg...
2                                 aniaL .ssiM ,nenikkieH
3           )leeP yaM yliL( htaeH seuqcaJ .srM ,ellertuF
4                               yrneH mailliW .rM ,nellA
                             ...                        
886                                sazouJ .veR ,alivtnoM
887                         htidE teragraM .ssiM ,maharG
888             "eirraC" neleH enirehtaC .ssiM ,notsnhoJ
889                                llewoH lraK .rM ,rheB
890                                  kcirtaP .rM ,yelooD
Name: Name, Length: 891, dtype: object


       

##################################################################################






Types of Data
Numerical Data
Categorical Data

# import the library
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

plt.style.use('default')
     
2D Line plot
image.png

Bivariate Analysis
categorical -> numerical and numerical -> numerical
Use case - Time series data

# plotting a simple function
price = [48000,54000,57000,49000,47000,45000]
year = [2015,2016,2017,2018,2019,2020]

plt.plot(year,price)
     
[<matplotlib.lines.Line2D at 0x7fb5d79e54f0>]


# from a pandas dataframe
batsman = pd.read_csv('/content/sharma-kohli.csv')
batsman

plt.plot(batsman['index'],batsman['V Kohli'])
     
[<matplotlib.lines.Line2D at 0x7fb5d682a220>]


# plotting multiple plots
plt.plot(batsman['index'],batsman['V Kohli'])
plt.plot(batsman['index'],batsman['RG Sharma'])
     
[<matplotlib.lines.Line2D at 0x7fb5d66f6fa0>]


# labels title
plt.plot(batsman['index'],batsman['V Kohli'])
plt.plot(batsman['index'],batsman['RG Sharma'])

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')
     
Text(0, 0.5, 'Runs Scored')


# colors(hex) and line(width and style) and marker(size)
plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F')
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6')

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')
     
Text(0, 0.5, 'Runs Scored')


plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F',linestyle='solid',linewidth=3)
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6',linestyle='dashdot',linewidth=2)

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')
     
Text(0, 0.5, 'Runs Scored')


plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F',linestyle='solid',linewidth=3,marker='D',markersize=10)
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6',linestyle='dashdot',linewidth=2,marker='o')

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')
     
Text(0, 0.5, 'Runs Scored')


# legend -> location
plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F',linestyle='solid',linewidth=3,marker='D',markersize=10,label='Virat')
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6',linestyle='dashdot',linewidth=2,marker='o',label='Rohit')

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')

plt.legend(loc='upper right')
     
<matplotlib.legend.Legend at 0x7fb5d60124f0>


# limiting axes
price = [48000,54000,57000,49000,47000,45000,4500000]
year = [2015,2016,2017,2018,2019,2020,2021]

plt.plot(year,price)
plt.ylim(0,75000)
plt.xlim(2017,2019)
     
(2017.0, 2019.0)


# grid
plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F',linestyle='solid',linewidth=3,marker='D',markersize=10)
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6',linestyle='dashdot',linewidth=2,marker='o')

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')

plt.grid()
     


# show
plt.plot(batsman['index'],batsman['V Kohli'],color='#D9F10F',linestyle='solid',linewidth=3,marker='D',markersize=10)
plt.plot(batsman['index'],batsman['RG Sharma'],color='#FC00D6',linestyle='dashdot',linewidth=2,marker='o')

plt.title('Rohit Sharma Vs Virat Kohli Career Comparison')
plt.xlabel('Season')
plt.ylabel('Runs Scored')

plt.grid()

plt.show()
     

Scatter Plots
image.png

Bivariate Analysis
numerical vs numerical
Use case - Finding correlation

# plt.scatter simple function
x = np.linspace(-10,10,50)

y = 10*x + 3 + np.random.randint(0,300,50)
y
     
array([199.        ,  70.08163265,  13.16326531,  25.24489796,
       198.32653061,  40.40816327, -64.51020408, 206.57142857,
       -60.34693878, -28.26530612,  23.81632653,  29.89795918,
         6.97959184, 166.06122449, 136.14285714, 156.2244898 ,
        -8.69387755, 204.3877551 ,  66.46938776,  85.55102041,
       203.63265306, 182.71428571, 139.79591837, 164.87755102,
        67.95918367,  57.04081633, 190.12244898,  51.20408163,
       101.28571429,  84.36734694,  31.44897959,  47.53061224,
       223.6122449 , 145.69387755, 278.7755102 , 122.85714286,
       258.93877551, 174.02040816, 315.10204082, 338.18367347,
       363.26530612, 242.34693878, 342.42857143, 376.51020408,
        98.59183673, 376.67346939,  95.75510204, 268.83673469,
       309.91836735, 324.        ])

plt.scatter(x,y)
     
<matplotlib.collections.PathCollection at 0x7fb5d5da8850>


# plt.scatter on pandas data
df = pd.read_csv('/content/batter.csv')
df = df.head(50)
df
     
batter	runs	avg	strike_rate
0	V Kohli	6634	36.251366	125.977972
1	S Dhawan	6244	34.882682	122.840842
2	DA Warner	5883	41.429577	136.401577
3	RG Sharma	5881	30.314433	126.964594
4	SK Raina	5536	32.374269	132.535312
5	AB de Villiers	5181	39.853846	148.580442
6	CH Gayle	4997	39.658730	142.121729
7	MS Dhoni	4978	39.196850	130.931089
8	RV Uthappa	4954	27.522222	126.152279
9	KD Karthik	4377	26.852761	129.267572
10	G Gambhir	4217	31.007353	119.665153
11	AT Rayudu	4190	28.896552	124.148148
12	AM Rahane	4074	30.863636	117.575758
13	KL Rahul	3895	46.927711	132.799182
14	SR Watson	3880	30.793651	134.163209
15	MK Pandey	3657	29.731707	117.739858
16	SV Samson	3526	29.140496	132.407060
17	KA Pollard	3437	28.404959	140.457703
18	F du Plessis	3403	34.373737	127.167414
19	YK Pathan	3222	29.290909	138.046272
20	BB McCullum	2882	27.711538	126.848592
21	RR Pant	2851	34.768293	142.550000
22	PA Patel	2848	22.603175	116.625717
23	JC Buttler	2832	39.333333	144.859335
24	SS Iyer	2780	31.235955	121.132898
25	Q de Kock	2767	31.804598	130.951254
26	Yuvraj Singh	2754	24.810811	124.784776
27	V Sehwag	2728	27.555556	148.827059
28	SA Yadav	2644	29.707865	134.009123
29	M Vijay	2619	25.930693	118.614130
30	RA Jadeja	2502	26.617021	122.108346
31	SPD Smith	2495	34.652778	124.812406
32	SE Marsh	2489	39.507937	130.109775
33	DA Miller	2455	36.102941	133.569097
34	JH Kallis	2427	28.552941	105.936272
35	WP Saha	2427	25.281250	124.397745
36	DR Smith	2385	28.392857	132.279534
37	MA Agarwal	2335	22.669903	129.506378
38	SR Tendulkar	2334	33.826087	114.187867
39	GJ Maxwell	2320	25.494505	147.676639
40	N Rana	2181	27.961538	130.053667
41	R Dravid	2174	28.233766	113.347237
42	KS Williamson	2105	36.293103	123.315759
43	AJ Finch	2092	24.904762	123.349057
44	AC Gilchrist	2069	27.223684	133.054662
45	AD Russell	2039	29.985294	168.234323
46	JP Duminy	2029	39.784314	120.773810
47	MEK Hussey	1977	38.764706	119.963592
48	HH Pandya	1972	29.878788	140.256046
49	Shubman Gill	1900	32.203390	122.186495

plt.scatter(df['avg'],df['strike_rate'],color='red',marker='+')
plt.title('Avg and SR analysis of Top 50 Batsman')
plt.xlabel('Average')
plt.ylabel('SR')
     
Text(0, 0.5, 'SR')


# marker
     

# size
tips = sns.load_dataset('tips')


# slower
plt.scatter(tips['total_bill'],tips['tip'],s=tips['size']*20)
     
<matplotlib.collections.PathCollection at 0x7fb5d597f550>


# scatterplot using plt.plot
# faster
plt.plot(tips['total_bill'],tips['tip'],'o')
     
[<matplotlib.lines.Line2D at 0x7fb5d591ac10>]


# plt.plot vs plt.scatter
     
Bar chart
image.png

Bivariate Analysis
Numerical vs Categorical
Use case - Aggregate analysis of groups

# simple bar chart
children = [10,20,40,10,30]
colors = ['red','blue','green','yellow','pink']

plt.bar(colors,children,color='black')
     
<BarContainer object of 5 artists>


# bar chart using data
     

# horizontal bar chart
plt.barh(colors,children,color='black')
     
<BarContainer object of 5 artists>


# color and label
df = pd.read_csv('/content/batsman_season_record.csv')
df
     
batsman	2015	2016	2017
0	AB de Villiers	513	687	216
1	DA Warner	562	848	641
2	MS Dhoni	372	284	290
3	RG Sharma	482	489	333
4	V Kohli	505	973	308

plt.bar(np.arange(df.shape[0]) - 0.2,df['2015'],width=0.2,color='yellow')
plt.bar(np.arange(df.shape[0]),df['2016'],width=0.2,color='red')
plt.bar(np.arange(df.shape[0]) + 0.2,df['2017'],width=0.2,color='blue')

plt.xticks(np.arange(df.shape[0]), df['batsman'])

plt.show()
     


np.arange(df.shape[0])
     
array([0, 1, 2, 3, 4])

# Multiple Bar charts
     

# xticks
     

# a problem
children = [10,20,40,10,30]
colors = ['red red red red red red','blue blue blue blue','green green green green green','yellow yellow yellow yellow ','pink pinkpinkpink']

plt.bar(colors,children,color='black')
plt.xticks(rotation='vertical')
     
([0, 1, 2, 3, 4], <a list of 5 Text major ticklabel objects>)


# Stacked Bar chart
plt.bar(df['batsman'],df['2017'],label='2017')
plt.bar(df['batsman'],df['2016'],bottom=df['2017'],label='2016')
plt.bar(df['batsman'],df['2015'],bottom=(df['2016'] + df['2017']),label='2015')

plt.legend()
plt.show()
     
---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3360             try:
-> 3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

/usr/local/lib/python3.8/dist-packages/pandas/_libs/index.pyx in pandas._libs.index.IndexEngine.get_loc()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

KeyError: '2017'

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
<ipython-input-280-5c1da7ceedc3> in <module>
      1 # Stacked Bar chart
----> 2 plt.bar(df['batsman'],df['2017'],label='2017')
      3 plt.bar(df['batsman'],df['2016'],bottom=df['2017'],label='2016')
      4 plt.bar(df['batsman'],df['2015'],bottom=(df['2016'] + df['2017']),label='2015')
      5 

/usr/local/lib/python3.8/dist-packages/pandas/core/frame.py in __getitem__(self, key)
   3456             if self.columns.nlevels > 1:
   3457                 return self._getitem_multilevel(key)
-> 3458             indexer = self.columns.get_loc(key)
   3459             if is_integer(indexer):
   3460                 indexer = [indexer]

/usr/local/lib/python3.8/dist-packages/pandas/core/indexes/base.py in get_loc(self, key, method, tolerance)
   3361                 return self._engine.get_loc(casted_key)
   3362             except KeyError as err:
-> 3363                 raise KeyError(key) from err
   3364 
   3365         if is_scalar(key) and isna(key) and not self.hasnans:

KeyError: '2017'
Histogram
image.png

Univariate Analysis
Numerical col
Use case - Frequency Count

# simple data

data = [32,45,56,10,15,27,61]

plt.hist(data,bins=[10,25,40,55,70])
     
(array([2., 2., 1., 2.]),
 array([10, 25, 40, 55, 70]),
 <a list of 4 Patch objects>)


# on some data
df = pd.read_csv('/content/vk.csv')
df
     
match_id	batsman_runs
0	12	62
1	17	28
2	20	64
3	27	0
4	30	10
...	...	...
136	624	75
137	626	113
138	632	54
139	633	0
140	636	54
141 rows × 2 columns


plt.hist(df['batsman_runs'],bins=[0,10,20,30,40,50,60,70,80,90,100,110,120])
plt.show()
     


# handling bins
     

# logarithmic scale
arr = np.load('/content/big-array.npy')
plt.hist(arr,bins=[10,20,30,40,50,60,70],log=True)
plt.show()
     

Pie Chart
image.png

Univariate/Bivariate Analysis
Categorical vs numerical
Use case - To find contibution on a standard scale

# simple data
data = [23,45,100,20,49]
subjects = ['eng','science','maths','sst','hindi']
plt.pie(data,labels=subjects)

plt.show()
     


# dataset
df = pd.read_csv('/content/gayle-175.csv')
df
     
batsman	batsman_runs
0	AB de Villiers	31
1	CH Gayle	175
2	R Rampaul	0
3	SS Tiwary	2
4	TM Dilshan	33
5	V Kohli	11

plt.pie(df['batsman_runs'],labels=df['batsman'],autopct='%0.1f%%')
plt.show()
     


# percentage and colors
plt.pie(df['batsman_runs'],labels=df['batsman'],autopct='%0.1f%%',colors=['blue','green','yellow','pink','cyan','brown'])
plt.show()
     


# explode shadow
plt.pie(df['batsman_runs'],labels=df['batsman'],autopct='%0.1f%%',explode=[0.3,0,0,0,0,0.1],shadow=True)
plt.show()
     

Changing styles

plt.style.available
     
['Solarize_Light2',
 '_classic_test_patch',
 'bmh',
 'classic',
 'dark_background',
 'fast',
 'fivethirtyeight',
 'ggplot',
 'grayscale',
 'seaborn',
 'seaborn-bright',
 'seaborn-colorblind',
 'seaborn-dark',
 'seaborn-dark-palette',
 'seaborn-darkgrid',
 'seaborn-deep',
 'seaborn-muted',
 'seaborn-notebook',
 'seaborn-paper',
 'seaborn-pastel',
 'seaborn-poster',
 'seaborn-talk',
 'seaborn-ticks',
 'seaborn-white',
 'seaborn-whitegrid',
 'tableau-colorblind10']

plt.style.use('dark_background')
     

arr = np.load('/content/big-array.npy')
plt.hist(arr,bins=[10,20,30,40,50,60,70],log=True)
plt.show()
     

Save figure

arr = np.load('/content/big-array.npy')
plt.hist(arr,bins=[10,20,30,40,50,60,70],log=True)

plt.savefig('sample.png')
     

Checkout Doc on website


     
#################################################################################



import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
     
Colored Scatterplots

iris = pd.read_csv('iris.csv')
iris.sample(5)
     
Id	SepalLengthCm	SepalWidthCm	PetalLengthCm	PetalWidthCm	Species
9	10	4.9	3.1	1.5	0.1	Iris-setosa
73	74	6.1	2.8	4.7	1.2	Iris-versicolor
44	45	5.1	3.8	1.9	0.4	Iris-setosa
51	52	6.4	3.2	4.5	1.5	Iris-versicolor
104	105	6.5	3.0	5.8	2.2	Iris-virginica

iris['Species'] = iris['Species'].replace({'Iris-setosa':0,'Iris-versicolor':1,'Iris-virginica':2})
iris.sample(5)
     
Id	SepalLengthCm	SepalWidthCm	PetalLengthCm	PetalWidthCm	Species
87	88	6.3	2.3	4.4	1.3	1
20	21	5.4	3.4	1.7	0.2	0
56	57	6.3	3.3	4.7	1.6	1
140	141	6.7	3.1	5.6	2.4	2
141	142	6.9	3.1	5.1	2.3	2

plt.scatter(iris['SepalLengthCm'],iris['PetalLengthCm'],c=iris['Species'],cmap='jet',alpha=0.7)
plt.xlabel('Sepal Length')
plt.ylabel('Petal Length')
plt.colorbar()
     
<matplotlib.colorbar.Colorbar at 0x7f5e17170bb0>


# cmap and alpha
     
Plot size

plt.figure(figsize=(15,7))

plt.scatter(iris['SepalLengthCm'],iris['PetalLengthCm'],c=iris['Species'],cmap='jet',alpha=0.7)
plt.xlabel('Sepal Length')
plt.ylabel('Petal Length')
plt.colorbar()
     
<matplotlib.colorbar.Colorbar at 0x7f5e16ee4430>

Annotations

batters = pd.read_csv('batter.csv')
     

batters.shape
     
(605, 4)

sample_df = df.head(100).sample(25,random_state=5)
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-137-839dfd0bcf32> in <module>
----> 1 sample_df = df.head(100).sample(25,random_state=5)

/usr/local/lib/python3.8/dist-packages/pandas/core/generic.py in sample(self, n, frac, replace, weights, random_state, axis, ignore_index)
   5363             )
   5364 
-> 5365         locs = rs.choice(axis_length, size=n, replace=replace, p=weights)
   5366         result = self.take(locs, axis=axis)
   5367         if ignore_index:

mtrand.pyx in numpy.random.mtrand.RandomState.choice()

ValueError: Cannot take a larger sample than population when 'replace=False'

sample_df
     
batter	runs	avg	strike_rate
66	KH Pandya	1326	22.100000	132.203390
32	SE Marsh	2489	39.507937	130.109775
46	JP Duminy	2029	39.784314	120.773810
28	SA Yadav	2644	29.707865	134.009123
74	IK Pathan	1150	21.698113	116.751269
23	JC Buttler	2832	39.333333	144.859335
10	G Gambhir	4217	31.007353	119.665153
20	BB McCullum	2882	27.711538	126.848592
17	KA Pollard	3437	28.404959	140.457703
35	WP Saha	2427	25.281250	124.397745
97	ST Jayasuriya	768	27.428571	134.031414
37	MA Agarwal	2335	22.669903	129.506378
70	DJ Hooda	1237	20.278689	127.525773
40	N Rana	2181	27.961538	130.053667
60	SS Tiwary	1494	28.730769	115.724245
34	JH Kallis	2427	28.552941	105.936272
42	KS Williamson	2105	36.293103	123.315759
57	DJ Bravo	1560	22.608696	125.100241
12	AM Rahane	4074	30.863636	117.575758
69	D Padikkal	1260	28.000000	119.205298
94	SO Hetmyer	831	30.777778	144.020797
56	PP Shaw	1588	25.206349	143.580470
22	PA Patel	2848	22.603175	116.625717
39	GJ Maxwell	2320	25.494505	147.676639
24	SS Iyer	2780	31.235955	121.132898

plt.figure(figsize=(18,10))
plt.scatter(sample_df['avg'],sample_df['strike_rate'],s=sample_df['runs'])

for i in range(sample_df.shape[0]):
  plt.text(sample_df['avg'].values[i],sample_df['strike_rate'].values[i],sample_df['batter'].values[i])
     


x = [1,2,3,4]
y = [5,6,7,8]

plt.scatter(x,y)
plt.text(1,5,'Point 1')
plt.text(2,6,'Point 2')
plt.text(3,7,'Point 3')
plt.text(4,8,'Point 4',fontdict={'size':12,'color':'brown'})
     
Text(4, 8, 'Point 4')

Horizontal and Vertical lines

plt.figure(figsize=(18,10))
plt.scatter(sample_df['avg'],sample_df['strike_rate'],s=sample_df['runs'])

plt.axhline(130,color='red')
plt.axhline(140,color='green')
plt.axvline(30,color='red')

for i in range(sample_df.shape[0]):
  plt.text(sample_df['avg'].values[i],sample_df['strike_rate'].values[i],sample_df['batter'].values[i])
     

Subplots

# A diff way to plot graphs
batters.head()
     
batter	runs	avg	strike_rate
0	V Kohli	6634	36.251366	125.977972
1	S Dhawan	6244	34.882682	122.840842
2	DA Warner	5883	41.429577	136.401577
3	RG Sharma	5881	30.314433	126.964594
4	SK Raina	5536	32.374269	132.535312

plt.figure(figsize=(15,6))
plt.scatter(batters['avg'],batters['strike_rate'])
plt.title('Something')
plt.xlabel('Avg')
plt.ylabel('Strike Rate')

plt.show()
     


fig,ax = plt.subplots(figsize=(15,6))

ax.scatter(batters['avg'],batters['strike_rate'],color='red',marker='+')
ax.set_title('Something')
ax.set_xlabel('Avg')
ax.set_ylabel('Strike Rate')

fig.show()
     


# batter dataset
     

fig, ax = plt.subplots(nrows=2,ncols=1,sharex=True,figsize=(10,6))

ax[0].scatter(batters['avg'],batters['strike_rate'],color='red')
ax[1].scatter(batters['avg'],batters['runs'])

ax[0].set_title('Avg Vs Strike Rate')
ax[0].set_ylabel('Strike Rate')


ax[1].set_title('Avg Vs Runs')
ax[1].set_ylabel('Runs')
ax[1].set_xlabel('Avg')
     
Text(0.5, 0, 'Avg')


fig, ax = plt.subplots(nrows=2,ncols=2,figsize=(10,10))

ax[0,0].
ax[0,1].scatter(batters['avg'],batters['runs'])
ax[1,0].hist(batters['avg'])
ax[1,1].hist(batters['runs'])
     
(array([499.,  40.,  19.,  19.,   9.,   6.,   4.,   4.,   3.,   2.]),
 array([   0. ,  663.4, 1326.8, 1990.2, 2653.6, 3317. , 3980.4, 4643.8,
        5307.2, 5970.6, 6634. ]),
 <a list of 10 Patch objects>)


fig = plt.figure()

ax1 = fig.add_subplot(2,2,1)
ax1.scatter(batters['avg'],batters['strike_rate'],color='red')

ax2 = fig.add_subplot(2,2,2)
ax2.hist(batters['runs'])

ax3 = fig.add_subplot(2,2,3)
ax3.hist(batters['avg'])
     
(array([102., 125., 103.,  82.,  78.,  43.,  22.,  14.,   2.,   1.]),
 array([ 0.        ,  5.56666667, 11.13333333, 16.7       , 22.26666667,
        27.83333333, 33.4       , 38.96666667, 44.53333333, 50.1       ,
        55.66666667]),
 <a list of 10 Patch objects>)


fig, ax = plt.subplots(nrows=2,ncols=2,sharex=True,figsize=(10,10))

ax[1,1]
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e15913c10>

3D Scatter Plots

batters

fig = plt.figure()

ax = plt.subplot(projection='3d')

ax.scatter3D(batters['runs'],batters['avg'],batters['strike_rate'],marker='+')
ax.set_title('IPL batsman analysis')

ax.set_xlabel('Runs')
ax.set_ylabel('Avg')
ax.set_zlabel('SR')
     
Text(0.5, 0, 'SR')

3D Line Plot

x = [0,1,5,25]
y = [0,10,13,0]
z = [0,13,20,9]

fig = plt.figure()

ax = plt.subplot(projection='3d')

ax.scatter3D(x,y,z,s=[100,100,100,100])
ax.plot3D(x,y,z,color='red')
     
[<mpl_toolkits.mplot3d.art3d.Line3D at 0x7f5e14d13f10>]

3D Surface Plots

x = np.linspace(-10,10,100)
y = np.linspace(-10,10,100)
     

xx, yy = np.meshgrid(x,y)
     
(100, 100)

z = xx**2 + yy**2
z.shape
     
(100, 100)

fig = plt.figure(figsize=(12,8))

ax = plt.subplot(projection='3d')

p = ax.plot_surface(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e141ac970>


z = np.sin(xx) + np.cos(yy)

fig = plt.figure(figsize=(12,8))

ax = plt.subplot(projection='3d')

p = ax.plot_surface(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e14076be0>


z = np.sin(xx) + np.log(xx)

fig = plt.figure(figsize=(12,8))

ax = plt.subplot(projection='3d')

p = ax.plot_surface(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<ipython-input-229-bbcd37ea4152>:1: RuntimeWarning: invalid value encountered in log
  z = np.sin(xx) + np.log(xx)
<ipython-input-229-bbcd37ea4152>:7: UserWarning: Z contains NaN values. This may result in rendering artifacts.
  p = ax.plot_surface(xx,yy,z,cmap='viridis')
<matplotlib.colorbar.Colorbar at 0x7f5e139a4a00>


fig = plt.figure(figsize=(12,8))

ax = plt.subplot(projection='3d')

p = ax.plot_surface(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e136f8970>

Contour Plots

fig = plt.figure(figsize=(12,8))

ax = plt.subplot()

p = ax.contour(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e13580a30>


fig = plt.figure(figsize=(12,8))

ax = plt.subplot()

p = ax.contourf(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e14f202b0>


z = np.sin(xx) + np.cos(yy)

fig = plt.figure(figsize=(12,8))

ax = plt.subplot()

p = ax.contourf(xx,yy,z,cmap='viridis')
fig.colorbar(p)
     
<matplotlib.colorbar.Colorbar at 0x7f5e14d5a2e0>

Heatmap

delivery = pd.read_csv('/content/IPL_Ball_by_Ball_2008_2022.csv')
delivery.head()
     
ID	innings	overs	ballnumber	batter	bowler	non-striker	extra_type	batsman_run	extras_run	total_run	non_boundary	isWicketDelivery	player_out	kind	fielders_involved	BattingTeam
0	1312200	1	0	1	YBK Jaiswal	Mohammed Shami	JC Buttler	NaN	0	0	0	0	0	NaN	NaN	NaN	Rajasthan Royals
1	1312200	1	0	2	YBK Jaiswal	Mohammed Shami	JC Buttler	legbyes	0	1	1	0	0	NaN	NaN	NaN	Rajasthan Royals
2	1312200	1	0	3	JC Buttler	Mohammed Shami	YBK Jaiswal	NaN	1	0	1	0	0	NaN	NaN	NaN	Rajasthan Royals
3	1312200	1	0	4	YBK Jaiswal	Mohammed Shami	JC Buttler	NaN	0	0	0	0	0	NaN	NaN	NaN	Rajasthan Royals
4	1312200	1	0	5	YBK Jaiswal	Mohammed Shami	JC Buttler	NaN	0	0	0	0	0	NaN	NaN	NaN	Rajasthan Royals

temp_df = delivery[(delivery['ballnumber'].isin([1,2,3,4,5,6])) & (delivery['batsman_run']==6)]
     

grid = temp_df.pivot_table(index='overs',columns='ballnumber',values='batsman_run',aggfunc='count')
     

plt.figure(figsize=(20,10))
plt.imshow(grid)
plt.yticks(delivery['overs'].unique(), list(range(1,21)))
plt.xticks(np.arange(0,6), list(range(1,7)))
plt.colorbar()
     
<matplotlib.colorbar.Colorbar at 0x7f5e12f98cd0>



     
Pandas Plot()

# on a series

s = pd.Series([1,2,3,4,5,6,7])
s.plot(kind='pie')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12f0a070>


# can be used on a dataframe as well
     

import seaborn as sns
tips = sns.load_dataset('tips')
     

tips['size'] = tips['size'] * 100
     


     

tips.head()
     
total_bill	tip	sex	smoker	day	time	size
0	16.99	1.01	Female	No	Sun	Dinner	200
1	10.34	1.66	Male	No	Sun	Dinner	300
2	21.01	3.50	Male	No	Sun	Dinner	300
3	23.68	3.31	Male	No	Sun	Dinner	200
4	24.59	3.61	Female	No	Sun	Dinner	400

# Scatter plot -> labels -> markers -> figsize -> color -> cmap
tips.plot(kind='scatter',x='total_bill',y='tip',title='Cost Analysis',marker='+',figsize=(10,6),s='size',c='sex',cmap='viridis')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12b4d760>


# 2d plot
# dataset = 'https://raw.githubusercontent.com/m-mehdi/pandas_tutorials/main/weekly_stocks.csv'

stocks = pd.read_csv('https://raw.githubusercontent.com/m-mehdi/pandas_tutorials/main/weekly_stocks.csv')
stocks.head()
     
Date	MSFT	FB	AAPL
0	2021-05-24	249.679993	328.730011	124.610001
1	2021-05-31	250.789993	330.350006	125.889999
2	2021-06-07	257.890015	331.260010	127.349998
3	2021-06-14	259.429993	329.660004	130.460007
4	2021-06-21	265.019989	341.369995	133.110001

# line plot
stocks['MSFT'].plot(kind='line')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12a55730>


stocks.plot(kind='line',x='Date')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e129f15e0>


stocks[['Date','AAPL','FB']].plot(kind='line',x='Date')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12950fa0>


# bar chart -> single -> horizontal -> multiple
# using tips
temp = pd.read_csv('/content/batsman_season_record.csv')
temp.head()
     
batsman	2015	2016	2017
0	AB de Villiers	513	687	216
1	DA Warner	562	848	641
2	MS Dhoni	372	284	290
3	RG Sharma	482	489	333
4	V Kohli	505	973	308

tips.groupby('sex')['total_bill'].mean().plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12350550>


temp['2015'].plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e123ceaf0>


temp.plot(kind='bar')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e1228fac0>


# stacked bar chart
temp.plot(kind='bar',stacked=True)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e12216e50>


# histogram
# using stocks

stocks[['MSFT','FB']].plot(kind='hist',bins=40)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e150247f0>


# pie -> single and multiple
df = pd.DataFrame(
    {
        'batsman':['Dhawan','Rohit','Kohli','SKY','Pandya','Pant'],
        'match1':[120,90,35,45,12,10],
        'match2':[0,1,123,130,34,45],
        'match3':[50,24,145,45,10,90]
    }
)

df.head()
     
batsman	match1	match2	match3
0	Dhawan	120	0	50
1	Rohit	90	1	24
2	Kohli	35	123	145
3	SKY	45	130	45
4	Pandya	12	34	10

df['match1'].plot(kind='pie',labels=df['batsman'].values,autopct='%0.1f%%')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f5e11e50790>


# multiple pie charts

df[['match1','match2','match3']].plot(kind='pie',subplots=True,figsize=(15,8))
     
array([<matplotlib.axes._subplots.AxesSubplot object at 0x7f5e11cc1b50>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e11c628b0>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e11c7ff10>],
      dtype=object)


# multiple separate graphs together
# using stocks

stocks.plot(kind='line',subplots=True)
     
array([<matplotlib.axes._subplots.AxesSubplot object at 0x7f5e11a2f7f0>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e11a50640>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e119f0d00>],
      dtype=object)


# on multiindex dataframes
# using tips
     

tips.pivot_table(index=['day','time'],columns=['sex','smoker'],values='total_bill',aggfunc='mean').plot(kind='pie',subplots=True,figsize=(20,10))
     
array([<matplotlib.axes._subplots.AxesSubplot object at 0x7f5e116a8cd0>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e115363d0>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e114d2a90>,
       <matplotlib.axes._subplots.AxesSubplot object at 0x7f5e1150b040>],
      dtype=object)


tips
     
total_bill	tip	sex	smoker	day	time	size
0	16.99	1.01	Female	No	Sun	Dinner	200
1	10.34	1.66	Male	No	Sun	Dinner	300
2	21.01	3.50	Male	No	Sun	Dinner	300
3	23.68	3.31	Male	No	Sun	Dinner	200
4	24.59	3.61	Female	No	Sun	Dinner	400
...	...	...	...	...	...	...	...
239	29.03	5.92	Male	No	Sat	Dinner	300
240	27.18	2.00	Female	Yes	Sat	Dinner	200
241	22.67	2.00	Male	Yes	Sat	Dinner	200
242	17.82	1.75	Male	No	Sat	Dinner	200
243	18.78	3.00	Female	No	Thur	Dinner	200
244 rows × 7 columns


stocks.plot(kind='scatter3D')
     
---------------------------------------------------------------------------
ValueError                                Traceback (most recent call last)
<ipython-input-321-4e91fa40f850> in <module>
----> 1 stocks.plot(kind='scatter3D')

/usr/local/lib/python3.8/dist-packages/pandas/plotting/_core.py in __call__(self, *args, **kwargs)
    903 
    904         if kind not in self._all_kinds:
--> 905             raise ValueError(f"{kind} is not a valid plot kind")
    906 
    907         # The original data structured can be transformed before passed to the

ValueError: scatter3D is not a valid plot kind


     
#################################################################################


Why Seaborn?
provides a layer of abstraction hence simpler to use
better aesthetics
more graphs included
Seaborn Roadmap
Types of Functions

Figure Level
Axis Level
Main Classification

Relational Plot
Distribution Plot
Categorical Plot
Regression Plot
Matrix Plot
Multiplots
https://seaborn.pydata.org/api.html

1. Relational Plot
to see the statistical relation between 2 or more variables.
Bivariate Analysis
Plots under this section

scatterplot
lineplot

import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
     

tips = sns.load_dataset('tips')
tips
     
total_bill	tip	sex	smoker	day	time	size
0	16.99	1.01	Female	No	Sun	Dinner	2
1	10.34	1.66	Male	No	Sun	Dinner	3
2	21.01	3.50	Male	No	Sun	Dinner	3
3	23.68	3.31	Male	No	Sun	Dinner	2
4	24.59	3.61	Female	No	Sun	Dinner	4
...	...	...	...	...	...	...	...
239	29.03	5.92	Male	No	Sat	Dinner	3
240	27.18	2.00	Female	Yes	Sat	Dinner	2
241	22.67	2.00	Male	Yes	Sat	Dinner	2
242	17.82	1.75	Male	No	Sat	Dinner	2
243	18.78	3.00	Female	No	Thur	Dinner	2
244 rows × 7 columns


# scatter plot -> axes level function
sns.scatterplot(data=tips, x='total_bill', y='tip',hue='sex',style='time',size='size')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f9585634670>


# relplot -> figure level -> square shape
sns.relplot(data=tips, x='total_bill', y='tip', kind='scatter',hue='sex',style='time',size='size')
     
<seaborn.axisgrid.FacetGrid at 0x7f9585625820>


# scatter using relplot -> size and hue
     

# style semantics
     

# line plot
gap = px.data.gapminder()
temp_df = gap[gap['country'] == 'India']
temp_df
     
country	continent	year	lifeExp	pop	gdpPercap	iso_alpha	iso_num
696	India	Asia	1952	37.373	372000000	546.565749	IND	356
697	India	Asia	1957	40.249	409000000	590.061996	IND	356
698	India	Asia	1962	43.605	454000000	658.347151	IND	356
699	India	Asia	1967	47.193	506000000	700.770611	IND	356
700	India	Asia	1972	50.651	567000000	724.032527	IND	356
701	India	Asia	1977	54.208	634000000	813.337323	IND	356
702	India	Asia	1982	56.596	708000000	855.723538	IND	356
703	India	Asia	1987	58.553	788000000	976.512676	IND	356
704	India	Asia	1992	60.223	872000000	1164.406809	IND	356
705	India	Asia	1997	61.765	959000000	1458.817442	IND	356
706	India	Asia	2002	62.879	1034172547	1746.769454	IND	356
707	India	Asia	2007	64.698	1110396331	2452.210407	IND	356

# axes level function
sns.lineplot(data=temp_df, x='year', y='lifeExp')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f95854b7c70>


# using relpplot
sns.relplot(data=temp_df, x='year', y='lifeExp', kind='line')
     
<seaborn.axisgrid.FacetGrid at 0x7f9585427a60>


# hue -> style
temp_df = gap[gap['country'].isin(['India','Brazil','Germany'])]
temp_df
     
country	continent	year	lifeExp	pop	gdpPercap	iso_alpha	iso_num
168	Brazil	Americas	1952	50.917	56602560	2108.944355	BRA	76
169	Brazil	Americas	1957	53.285	65551171	2487.365989	BRA	76
170	Brazil	Americas	1962	55.665	76039390	3336.585802	BRA	76
171	Brazil	Americas	1967	57.632	88049823	3429.864357	BRA	76
172	Brazil	Americas	1972	59.504	100840058	4985.711467	BRA	76
173	Brazil	Americas	1977	61.489	114313951	6660.118654	BRA	76
174	Brazil	Americas	1982	63.336	128962939	7030.835878	BRA	76
175	Brazil	Americas	1987	65.205	142938076	7807.095818	BRA	76
176	Brazil	Americas	1992	67.057	155975974	6950.283021	BRA	76
177	Brazil	Americas	1997	69.388	168546719	7957.980824	BRA	76
178	Brazil	Americas	2002	71.006	179914212	8131.212843	BRA	76
179	Brazil	Americas	2007	72.390	190010647	9065.800825	BRA	76
564	Germany	Europe	1952	67.500	69145952	7144.114393	DEU	276
565	Germany	Europe	1957	69.100	71019069	10187.826650	DEU	276
566	Germany	Europe	1962	70.300	73739117	12902.462910	DEU	276
567	Germany	Europe	1967	70.800	76368453	14745.625610	DEU	276
568	Germany	Europe	1972	71.000	78717088	18016.180270	DEU	276
569	Germany	Europe	1977	72.500	78160773	20512.921230	DEU	276
570	Germany	Europe	1982	73.800	78335266	22031.532740	DEU	276
571	Germany	Europe	1987	74.847	77718298	24639.185660	DEU	276
572	Germany	Europe	1992	76.070	80597764	26505.303170	DEU	276
573	Germany	Europe	1997	77.340	82011073	27788.884160	DEU	276
574	Germany	Europe	2002	78.670	82350671	30035.801980	DEU	276
575	Germany	Europe	2007	79.406	82400996	32170.374420	DEU	276
696	India	Asia	1952	37.373	372000000	546.565749	IND	356
697	India	Asia	1957	40.249	409000000	590.061996	IND	356
698	India	Asia	1962	43.605	454000000	658.347151	IND	356
699	India	Asia	1967	47.193	506000000	700.770611	IND	356
700	India	Asia	1972	50.651	567000000	724.032527	IND	356
701	India	Asia	1977	54.208	634000000	813.337323	IND	356
702	India	Asia	1982	56.596	708000000	855.723538	IND	356
703	India	Asia	1987	58.553	788000000	976.512676	IND	356
704	India	Asia	1992	60.223	872000000	1164.406809	IND	356
705	India	Asia	1997	61.765	959000000	1458.817442	IND	356
706	India	Asia	2002	62.879	1034172547	1746.769454	IND	356
707	India	Asia	2007	64.698	1110396331	2452.210407	IND	356

sns.relplot(kind='line', data=temp_df, x='year', y='lifeExp', hue='country', style='continent', size='continent')
     
<seaborn.axisgrid.FacetGrid at 0x7f9585258c70>


sns.lineplot(data=temp_df, x='year', y='lifeExp', hue='country')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f95853837c0>


# facet plot -> figure level function -> work with relplot
# it will not work with scatterplot and lineplot
sns.relplot(data=tips, x='total_bill', y='tip', kind='line', col='sex', row='day')
     
<seaborn.axisgrid.FacetGrid at 0x7f9584b8f8b0>


# col wrap
sns.relplot(data=gap, x='lifeExp', y='gdpPercap', kind='scatter', col='year', col_wrap=3)
     
<seaborn.axisgrid.FacetGrid at 0x7f95844efb80>


sns.scatterplot(data=tips, x='total_bill', y='tip', col='sex', row='day')
     
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-199-13fa0b1b528e> in <module>
----> 1 sns.scatterplot(data=tips, x='total_bill', y='tip', col='sex', row='day')

/usr/local/lib/python3.8/dist-packages/seaborn/_decorators.py in inner_f(*args, **kwargs)
     44             )
     45         kwargs.update({k: arg for k, arg in zip(sig.parameters, args)})
---> 46         return f(**kwargs)
     47     return inner_f
     48 

/usr/local/lib/python3.8/dist-packages/seaborn/relational.py in scatterplot(x, y, hue, style, size, data, palette, hue_order, hue_norm, sizes, size_order, size_norm, markers, style_order, x_bins, y_bins, units, estimator, ci, n_boot, alpha, x_jitter, y_jitter, legend, ax, **kwargs)
    825     p._attach(ax)
    826 
--> 827     p.plot(ax, kwargs)
    828 
    829     return ax

/usr/local/lib/python3.8/dist-packages/seaborn/relational.py in plot(self, ax, kws)
    606         )
    607         scout_x = scout_y = np.full(scout_size, np.nan)
--> 608         scout = ax.scatter(scout_x, scout_y, **kws)
    609         s = kws.pop("s", scout.get_sizes())
    610         c = kws.pop("c", scout.get_facecolors())

/usr/local/lib/python3.8/dist-packages/matplotlib/__init__.py in inner(ax, data, *args, **kwargs)
   1563     def inner(ax, *args, data=None, **kwargs):
   1564         if data is None:
-> 1565             return func(ax, *map(sanitize_sequence, args), **kwargs)
   1566 
   1567         bound = new_sig.bind(ax, *args, **kwargs)

/usr/local/lib/python3.8/dist-packages/matplotlib/cbook/deprecation.py in wrapper(*args, **kwargs)
    356                 f"%(removal)s.  If any parameter follows {name!r}, they "
    357                 f"should be pass as keyword, not positionally.")
--> 358         return func(*args, **kwargs)
    359 
    360     return wrapper

/usr/local/lib/python3.8/dist-packages/matplotlib/axes/_axes.py in scatter(self, x, y, s, c, marker, cmap, norm, vmin, vmax, alpha, linewidths, verts, edgecolors, plotnonfinite, **kwargs)
   4441                 )
   4442         collection.set_transform(mtransforms.IdentityTransform())
-> 4443         collection.update(kwargs)
   4444 
   4445         if colors is None:

/usr/local/lib/python3.8/dist-packages/matplotlib/artist.py in update(self, props)
   1004 
   1005         with cbook._setattr_cm(self, eventson=False):
-> 1006             ret = [_update_property(self, k, v) for k, v in props.items()]
   1007 
   1008         if len(ret):

/usr/local/lib/python3.8/dist-packages/matplotlib/artist.py in <listcomp>(.0)
   1004 
   1005         with cbook._setattr_cm(self, eventson=False):
-> 1006             ret = [_update_property(self, k, v) for k, v in props.items()]
   1007 
   1008         if len(ret):

/usr/local/lib/python3.8/dist-packages/matplotlib/artist.py in _update_property(self, k, v)
    999                 func = getattr(self, 'set_' + k, None)
   1000                 if not callable(func):
-> 1001                     raise AttributeError('{!r} object has no property {!r}'
   1002                                          .format(type(self).__name__, k))
   1003                 return func(v)

AttributeError: 'PathCollection' object has no property 'col'

2. Distribution Plots
used for univariate analysis
used to find out the distribution
Range of the observation
Central Tendency
is the data bimodal?
Are there outliers?
Plots under distribution plot

histplot
kdeplot
rugplot

# figure level -> displot
# axes level -> histplot -> kdeplot -> rugplot
     

# plotting univariate histogram
sns.histplot(data=tips, x='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f9583d7dbb0>


sns.displot(data=tips, x='total_bill', kind='hist')
     
<seaborn.axisgrid.FacetGrid at 0x7f9583ebc7f0>


# bins parameter
sns.displot(data=tips, x='total_bill', kind='hist',bins=2)
     
<seaborn.axisgrid.FacetGrid at 0x7f9583c93280>


# It’s also possible to visualize the distribution of a categorical variable using the logic of a histogram.
# Discrete bins are automatically set for categorical variables

# countplot
sns.displot(data=tips, x='day', kind='hist')
     
<seaborn.axisgrid.FacetGrid at 0x7f958517d9d0>


# hue parameter
sns.displot(data=tips, x='tip', kind='hist',hue='sex')
     
<seaborn.axisgrid.FacetGrid at 0x7f9583d05280>


# element -> step
sns.displot(data=tips, x='tip', kind='hist',hue='sex',element='step')
     
<seaborn.axisgrid.FacetGrid at 0x7f9583c2cfa0>


titanic = sns.load_dataset('titanic')
titanic
     
survived	pclass	sex	age	sibsp	parch	fare	embarked	class	who	adult_male	deck	embark_town	alive	alone
0	0	3	male	22.0	1	0	7.2500	S	Third	man	True	NaN	Southampton	no	False
1	1	1	female	38.0	1	0	71.2833	C	First	woman	False	C	Cherbourg	yes	False
2	1	3	female	26.0	0	0	7.9250	S	Third	woman	False	NaN	Southampton	yes	True
3	1	1	female	35.0	1	0	53.1000	S	First	woman	False	C	Southampton	yes	False
4	0	3	male	35.0	0	0	8.0500	S	Third	man	True	NaN	Southampton	no	True
...	...	...	...	...	...	...	...	...	...	...	...	...	...	...	...
886	0	2	male	27.0	0	0	13.0000	S	Second	man	True	NaN	Southampton	no	True
887	1	1	female	19.0	0	0	30.0000	S	First	woman	False	B	Southampton	yes	True
888	0	3	female	NaN	1	2	23.4500	S	Third	woman	False	NaN	Southampton	no	False
889	1	1	male	26.0	0	0	30.0000	C	First	man	True	C	Cherbourg	yes	True
890	0	3	male	32.0	0	0	7.7500	Q	Third	man	True	NaN	Queenstown	no	True
891 rows × 15 columns


sns.displot(data=titanic, x='age', kind='hist',element='step',hue='sex')
     
<seaborn.axisgrid.FacetGrid at 0x7f9583c1b9d0>


# faceting using col and row -> not work on histplot function
sns.displot(data=tips, x='tip', kind='hist',col='sex',element='step')
     
<seaborn.axisgrid.FacetGrid at 0x7f9583904850>


# kdeplot
# Rather than using discrete bins, a KDE plot smooths the observations with a Gaussian kernel, producing a continuous density estimate
sns.kdeplot(data=tips,x='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f958384a160>


sns.displot(data=tips,x='total_bill',kind='kde')
     
<seaborn.axisgrid.FacetGrid at 0x7f95838c2790>


# hue -> fill
sns.displot(data=tips,x='total_bill',kind='kde',hue='sex',fill=True,height=10,aspect=2)
     
<seaborn.axisgrid.FacetGrid at 0x7f95821a35b0>


# Rugplot

# Plot marginal distributions by drawing ticks along the x and y axes.

# This function is intended to complement other plots by showing the location of individual observations in an unobtrusive way.
sns.kdeplot(data=tips,x='total_bill')
sns.rugplot(data=tips,x='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f95836ff0d0>


# Bivariate histogram
# A bivariate histogram bins the data within rectangles that tile the plot
# and then shows the count of observations within each rectangle with the fill color

# sns.histplot(data=tips, x='total_bill', y='tip')
sns.displot(data=tips, x='total_bill', y='tip',kind='hist')
     
<seaborn.axisgrid.FacetGrid at 0x7f958362fc10>


# Bivariate Kdeplot
# a bivariate KDE plot smoothes the (x, y) observations with a 2D Gaussian
sns.kdeplot(data=tips, x='total_bill', y='tip')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f95835b60d0>

2. Matrix Plot
Heatmap
Clustermap

# Heatmap

# Plot rectangular data as a color-encoded matrix
temp_df = gap.pivot(index='country',columns='year',values='lifeExp')

# axes level function
plt.figure(figsize=(15,15))
sns.heatmap(temp_df)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f958387d910>


# annot
temp_df = gap[gap['continent'] == 'Europe'].pivot(index='country',columns='year',values='lifeExp')

plt.figure(figsize=(15,15))
sns.heatmap(temp_df,annot=True,linewidth=0.5, cmap='summer')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7f9584de7af0>


# linewidth
     

# cmap
     

# Clustermap

# Plot a matrix dataset as a hierarchically-clustered heatmap.

# This function requires scipy to be available.

iris = px.data.iris()
iris
     
sepal_length	sepal_width	petal_length	petal_width	species	species_id
0	5.1	3.5	1.4	0.2	setosa	1
1	4.9	3.0	1.4	0.2	setosa	1
2	4.7	3.2	1.3	0.2	setosa	1
3	4.6	3.1	1.5	0.2	setosa	1
4	5.0	3.6	1.4	0.2	setosa	1
...	...	...	...	...	...	...
145	6.7	3.0	5.2	2.3	virginica	3
146	6.3	2.5	5.0	1.9	virginica	3
147	6.5	3.0	5.2	2.0	virginica	3
148	6.2	3.4	5.4	2.3	virginica	3
149	5.9	3.0	5.1	1.8	virginica	3
150 rows × 6 columns


sns.clustermap(iris.iloc[:,[0,1,2,3]])
     
<seaborn.matrix.ClusterGrid at 0x7f958226c580>



     
#################################################################################



import seaborn as sns
     

# import datasets
tips = sns.load_dataset('tips')
iris = sns.load_dataset('iris')
     
Categorical Plots
Categorical Scatter Plot
Stripplot
Swarmplot
Categorical Distribution Plots
Boxplot
Violinplot
Categorical Estimate Plot -> for central tendency
Barplot
Pointplot
Countplot
Figure level function -> catplot

sns.scatterplot(data=tips, x='total_bill',y='tip')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc63bca430>


# strip plot
# axes level function
sns.stripplot(data=tips,x='day',y='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc63ae0790>


# using catplot
# figure level function
sns.catplot(data=tips, x='day',y='total_bill',kind='strip')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc63ab2610>


# jitter
sns.catplot(data=tips, x='day',y='total_bill',kind='strip',jitter=0.2,hue='sex')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc63f700a0>


# swarmplot
sns.catplot(data=tips, x='day',y='total_bill',kind='swarm')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc63808670>


sns.swarmplot(data=tips, x='day',y='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc637d6520>


# hue
sns.swarmplot(data=tips, x='day',y='total_bill',hue='sex')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc63748dc0>

Boxplot
A boxplot is a standardized way of displaying the distribution of data based on a five number summary (“minimum”, first quartile [Q1], median, third quartile [Q3] and “maximum”). It can tell you about your outliers and what their values are. Boxplots can also tell you if your data is symmetrical, how tightly your data is grouped and if and how your data is skewed.

image.png


# Box plot
sns.boxplot(data=tips,x='day',y='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc6369a1c0>


# Using catplot
sns.catplot(data=tips,x='day',y='total_bill',kind='box')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc638849a0>


# hue
sns.boxplot(data=tips,x='day',y='total_bill',hue='sex')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc64d03490>


# single boxplot -> numerical col
sns.boxplot(data=tips,y='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc63588280>

Violinplot = (Boxplot + KDEplot)

# violinplot
sns.violinplot(data=tips,x='day',y='total_bill')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc63576f40>


sns.catplot(data=tips,x='day',y='total_bill',kind='violin')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc635084f0>


# hue

sns.catplot(data=tips,x='day',y='total_bill',kind='violin',hue='sex',split=True)
     
<seaborn.axisgrid.FacetGrid at 0x7fcc635085e0>


# barplot
# some issue with errorbar
import numpy as np
sns.barplot(data=tips, x='sex', y='total_bill',hue='smoker',estimator=np.min)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc62f23a00>


sns.barplot(data=tips, x='sex', y='total_bill',ci=None)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc634468e0>


# point plot
sns.pointplot(data=tips, x='sex', y='total_bill',hue='smoker',ci=None)
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc634a3100>

When there are multiple observations in each category, it also uses bootstrapping to compute a confidence interval around the estimate, which is plotted using error bars


# countplot
sns.countplot(data=tips,x='sex',hue='day')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc62c552b0>

A special case for the bar plot is when you want to show the number of observations in each category rather than computing a statistic for a second variable. This is similar to a histogram over a categorical, rather than quantitative, variable


# pointplot
     

# faceting using catplot
sns.catplot(data=tips, x='sex',y='total_bill',col='smoker',kind='box',row='time')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc62b8ee80>

Regression Plots
regplot
lmplot
In the simplest invocation, both functions draw a scatterplot of two variables, x and y, and then fit the regression model y ~ x and plot the resulting regression line and a 95% confidence interval for that regression.


# axes level
# hue parameter is not available
sns.regplot(data=tips,x='total_bill',y='tip')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc62829550>


sns.lmplot(data=tips,x='total_bill',y='tip',hue='sex')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc627f1250>


# residplot
sns.residplot(data=tips,x='total_bill',y='tip')
     
<matplotlib.axes._subplots.AxesSubplot at 0x7fcc62755d30>



     


     
A second way to plot Facet plots -> FacetGrid

# figure level -> relplot -> displot -> catplot -> lmplot
sns.catplot(data=tips,x='sex',y='total_bill',kind='violin',col='day',row='time')
     
<seaborn.axisgrid.FacetGrid at 0x7fcc62538970>


g = sns.FacetGrid(data=tips,col='day',row='time',hue='smoker')
g.map(sns.boxplot,'sex','total_bill')
g.add_legend()
     
/usr/local/lib/python3.8/dist-packages/seaborn/axisgrid.py:670: UserWarning: Using the boxplot function without specifying `order` is likely to produce an incorrect plot.
  warnings.warn(warning)
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
<ipython-input-134-983b773fc0d8> in <module>
      1 g = sns.FacetGrid(data=tips,col='day',row='time',hue='smoker')
----> 2 g.map(sns.boxplot,'sex','total_bill')
      3 g.add_legend()

/usr/local/lib/python3.8/dist-packages/seaborn/axisgrid.py in map(self, func, *args, **kwargs)
    708 
    709             # Draw the plot
--> 710             self._facet_plot(func, ax, plot_args, kwargs)
    711 
    712         # Finalize the annotations and layout

/usr/local/lib/python3.8/dist-packages/seaborn/axisgrid.py in _facet_plot(self, func, ax, plot_args, plot_kwargs)
    804             plot_args = []
    805             plot_kwargs["ax"] = ax
--> 806         func(*plot_args, **plot_kwargs)
    807 
    808         # Sort out the supporting information

/usr/local/lib/python3.8/dist-packages/seaborn/_decorators.py in inner_f(*args, **kwargs)
     44             )
     45         kwargs.update({k: arg for k, arg in zip(sig.parameters, args)})
---> 46         return f(**kwargs)
     47     return inner_f
     48 

/usr/local/lib/python3.8/dist-packages/seaborn/categorical.py in boxplot(x, y, hue, data, order, hue_order, orient, color, palette, saturation, width, dodge, fliersize, linewidth, whis, ax, **kwargs)
   2249     kwargs.update(dict(whis=whis))
   2250 
-> 2251     plotter.plot(ax, kwargs)
   2252     return ax
   2253 

/usr/local/lib/python3.8/dist-packages/seaborn/categorical.py in plot(self, ax, boxplot_kws)
    507     def plot(self, ax, boxplot_kws):
    508         """Make the plot."""
--> 509         self.draw_boxplot(ax, boxplot_kws)
    510         self.annotate_axes(ax)
    511         if self.orient == "h":

/usr/local/lib/python3.8/dist-packages/seaborn/categorical.py in draw_boxplot(self, ax, kws)
    439                     continue
    440 
--> 441                 artist_dict = ax.boxplot(box_data,
    442                                          vert=vert,
    443                                          patch_artist=True,

/usr/local/lib/python3.8/dist-packages/matplotlib/cbook/deprecation.py in wrapper(*args, **kwargs)
    294                 f"for the old name will be dropped %(removal)s.")
    295             kwargs[new] = kwargs.pop(old)
--> 296         return func(*args, **kwargs)
    297 
    298     # wrapper() must keep the same documented signature as func(): if we

/usr/local/lib/python3.8/dist-packages/matplotlib/__init__.py in inner(ax, data, *args, **kwargs)
   1563     def inner(ax, *args, data=None, **kwargs):
   1564         if data is None:
-> 1565             return func(ax, *map(sanitize_sequence, args), **kwargs)
   1566 
   1567         bound = new_sig.bind(ax, *args, **kwargs)

TypeError: boxplot() got an unexpected keyword argument 'label'


# height and aspect
     
<seaborn.axisgrid.FacetGrid at 0x7f8b2a2f3070>


     


     
Plotting Pairwise Relationship (PairGrid Vs Pairplot)

sns.pairplot(iris,hue='species')
     
<seaborn.axisgrid.PairGrid at 0x7fcc60bdcac0>



     

# pair grid
g = sns.PairGrid(data=iris,hue='species')
# g.map
g.map(sns.scatterplot)
     
<seaborn.axisgrid.PairGrid at 0x7fcc5fd0d700>


# map_diag -> map_offdiag
g = sns.PairGrid(data=iris,hue='species')
g.map_diag(sns.boxplot)
g.map_offdiag(sns.kdeplot)
     
<seaborn.axisgrid.PairGrid at 0x7fcc5e28da60>


# map_diag -> map_upper -> map_lower
g = sns.PairGrid(data=iris,hue='species')
g.map_diag(sns.histplot)
g.map_upper(sns.kdeplot)
g.map_lower(sns.scatterplot)
     
<seaborn.axisgrid.PairGrid at 0x7fcc5daaa880>


# vars
g = sns.PairGrid(data=iris,hue='species',vars=['sepal_width','petal_width'])
g.map_diag(sns.histplot)
g.map_upper(sns.kdeplot)
g.map_lower(sns.scatterplot)
     
<seaborn.axisgrid.PairGrid at 0x7fcc5ea01790>

JointGrid Vs Jointplot

sns.jointplot(data=tips,x='total_bill',y='tip',kind='hist',hue='sex')
     
<seaborn.axisgrid.JointGrid at 0x7fcc5c8c6070>


g = sns.JointGrid(data=tips,x='total_bill',y='tip')
g.plot(sns.kdeplot,sns.violinplot)
     
<seaborn.axisgrid.JointGrid at 0x7fcc5bf817f0>



     


     


     


     


     


     
Utility Functions

# get dataset names
sns.get_dataset_names()
     
['anagrams',
 'anscombe',
 'attention',
 'brain_networks',
 'car_crashes',
 'diamonds',
 'dots',
 'dowjones',
 'exercise',
 'flights',
 'fmri',
 'geyser',
 'glue',
 'healthexp',
 'iris',
 'mpg',
 'penguins',
 'planets',
 'seaice',
 'taxis',
 'tips',
 'titanic']

# load dataset
sns.load_dataset('planets')
     
method	number	orbital_period	mass	distance	year
0	Radial Velocity	1	269.300000	7.10	77.40	2006
1	Radial Velocity	1	874.774000	2.21	56.95	2008
2	Radial Velocity	1	763.000000	2.60	19.84	2011
3	Radial Velocity	1	326.030000	19.40	110.62	2007
4	Radial Velocity	1	516.220000	10.50	119.47	2009
...	...	...	...	...	...	...
1030	Transit	1	3.941507	NaN	172.00	2006
1031	Transit	1	2.615864	NaN	148.00	2007
1032	Transit	1	3.191524	NaN	174.00	2007
1033	Transit	1	4.125083	NaN	293.00	2008
1034	Transit	1	4.187757	NaN	260.00	2008
1035 rows × 6 columns



     