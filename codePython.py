# import OS module
import os

# This is my path
path = "D://finacusjobhackathon//CodingVideos//campusX python DSA//W3Resource-Python-CPP-Java-master//Python"

# to store files in a list
list1 = []

# dirs=directories
for (root, dirs, file) in os.walk(path):
    for f in file:
        if '.py' in f:
            # print(f)
            list1.append(f)
            
print(list1)


list2=[int(x) for x in list2]
list2.sort()
list2=[str(x) for x in list2]


for i in list1:
    with open(i,'r') as file:
    # with open(str(i)+'.py','r') as file:
        # file = open('code.py','r')
        data = file.read()
        file.close()
        # f = open('code.py','w')
        f = open('code.py','a')
        f.write("\n")
        f.write("\n")
        f.write("\n")
        f.write("\n")
        f.write(str(i) + " " + data)
        f.close()
        


# search for .py



Basic Exercises I.py # 1. Write a Python program to print the following string in a specific format (see the output).
# Sample String : "Twinkle, twinkle, little star, How I wonder what you are! Up above the world so high, Like a diamond in the sky. Twinkle, twinkle, little star, How I wonder what you are" Output :

# Twinkle, twinkle, little star,
# 	How I wonder what you are!
# 		Up above the world so high,
# 		Like a diamond in the sky.
# Twinkle, twinkle, little star,
# 	How I wonder what you are

print("Twinkle, twinkle, little star, \n\tHow I wonder what you are! \n\t\tUp above the world so high, \n\t\tLike a diamond in the sky. \nTwinkle, twinkle, little star, \n\tHow I wonder what you are!")

# 2. Write a Python program to get the Python version you are using.

import sys
print("Python version")
print(sys.version)
print("Version info.")
print(sys.version_info)

# 3. Write a Python program to display the current date and time.
# Sample Output :
# Current date and time :
# 2014-07-05 14:34:14

import datetime
now = datetime.datetime.now()
print("Current date and time : ")
print(now.strftime("%Y-%m-%d %H:%M:%S"))

# 4. Write a Python program which accepts the radius of a circle from the user and compute the area.
# Sample Output :
# r = 1.1
# Area = 3.8013271108436504
from math import pi
r = float(input("Input the radius of the circle : "))
print(f"The area of the circle with radius {r} is: {str(pi * r**2)}")


# 5. Write a Python program which accepts the user's first and last name and print them in reverse order with a space between them.
fname = input("Input your First Name : ")
lname = input("Input your Last Name : ")
print(f"Hello  {lname} {fname}")


# 6. Write a Python program which accepts a sequence of comma-separated numbers from user and generate a list and a tuple with those numbers.
# Sample data : 3, 5, 7, 23
# Output :
# List : ['3', ' 5', ' 7', ' 23']
# Tuple : ('3', ' 5', ' 7', ' 23')

color_list = ["Red", "Green", "White", "Black"]
print(f"{color_list[0]} {color_list[-1]}")

# 7. Write a Python program to accept a filename from the user and print the extension of that.
# Sample filename : abc.java
# Output : java
filename = input("Input the Filename: ")
f_extns = filename.split(".")
print(f"The extension of the file is : {repr(f_extns[-1])}")


# 8. Write a Python program to display the first and last colors from the following list.
# color_list = ["Red","Green","White" ,"Black"]
color_list = ["Red", "Green", "White", "Black"]
print(f"{color_list[0]} {color_list[-1]}")


# 9. Write a Python program to display the examination schedule. (extract the date from exam_st_date).
# exam_st_date = (11, 12, 2014)
# Sample Output : The examination will start from : 11 / 12 / 2014

exam_st_date = (11, 12, 2014)
print("The examination will start from : %i / %i / %i" % exam_st_date)

# 10. Write a Python program that accepts an integer (n) and computes the value of n+nn+nnn.
# Sample value of n is 5
# Expected Result : 615

a = int(input("Input an integer : "))
n1 = int(f"{a}")
n2 = int(f"{a}{a}")
n3 = int(f"{a}{a}{a}")
print(n1 + n2 + n3)

# 11. Write a Python program to print the documents (syntax, description etc.) of Python built-in function(s).
# Sample function : abs()
# Expected Result :
# abs(number) -> number
# Return the absolute value of the argument.
print(abs.__doc__)

# 12. Write a Python program to print the calendar of a given month and year.
# Note : Use 'calendar' module.
import calendar
y = int(input("Input the year : "))
m = int(input("Input the month : "))
print(calendar.month(y, m))

# 13. Write a Python program to print the following here document.
# Sample string :
# a string that you "don't" have to escape
# This
# is a ....... multi-line
# heredoc string --------> example
print("""
a string that you "don't" have to escape
This
is a  ....... multi-line
heredoc string --------> example
""")


# 14. Write a Python program to calculate number of days between two dates.
# Sample dates : (2014, 7, 2), (2014, 7, 11)
# Expected output : 9 days

from datetime import date
f_date = date(2014, 7, 2)
l_date = date(2014, 7, 11)
delta = l_date - f_date
print(delta.days)

# 15. Write a Python program to get the volume of a sphere with radius 6.

pi = 3.1415926535897931
r = 6.0
V = 4.0 / 3.0 * pi * r**3
print('The volume of the sphere is: ', V)

# 16. Write a Python program to get the difference between a given number and 17, if the number is greater than 17 return double the absolute difference.


def difference(n):
    return 17 - n if n <= 17 else (n - 17) * 2


print(difference(22))
print(difference(14))

# 17. Write a Python program to test whether a number is within 100 of 1000 or 2000.

def near_thousand(n):
    return ((abs(1000 - n) <= 100) or (abs(2000 - n) <= 100))

print(near_thousand(1000))
print(near_thousand(900))
print(near_thousand(800))
print(near_thousand(2200))


# 18. Write a Python program to calculate the sum of three given numbers, if the values are equal then return three times of their sum.
def sum_thrice(x, y, z):

    sum = x + y + z

    if x == y == z:
        sum = sum * 3
    return sum


print(sum_thrice(1, 2, 3))
print(sum_thrice(3, 3, 3))


# 19. Write a Python program to get a new string from a given string where "Is" has been added to the front. If the given string already begins with "Is" then return the string unchanged.
def new_string(str):
    return str if len(str) >= 2 and str[:2] == "Is" else f"Is{str}"


print(new_string("Array"))
print(new_string("IsEmpty"))


# 20. Write a Python program to get a string which is n (non-negative integer) copies of a given string.
def larger_string(str, n):
    return str * n

print(larger_string('abc', 2))
print(larger_string('.py', 3))


# 21. Write a Python program to find whether a given number (accept from the user) is even or odd, print out an appropriate message to the user.
num = int(input("Enter a number: "))
if num % 2:
    print("This is an odd number.")
else:
    print("This is an even number.")

# 22. Write a Python program to count the number 4 in a given list.
def list_count_4(nums):
    return nums.count(4)

print(list_count_4([1, 4, 6, 7, 4]))
print(list_count_4([1, 4, 6, 4, 7, 4]))

# 23. Write a Python program to get the n (non-negative integer) copies of the first 2 characters of a given string. Return the n copies of the whole string if the length is less than 2.

def substring_copy(str, n):
    flen = 2
    flen = min(flen, len(str))
    substr = str[:flen]

    result = ""
    for _ in range(n):
        result = result + substr
    return result


print(substring_copy('abcdef', 2))
print(substring_copy('p', 3))


# 24. Write a Python program to test whether a passed letter is a vowel or not.

def is_vowel(char):
    all_vowels = 'aeiou'
    return char in all_vowels


print(is_vowel('c'))
print(is_vowel('e'))

# 25. Write a Python program to check whether a specified value is contained in a group of values.
# Test Data :
# 3 -> [1, 5, 8, 3] : True
# -1 -> [1, 5, 8, 3] : False
def is_group_member(group_data, n):
    return n in group_data

print(is_group_member([1, 5, 8, 3], 3))
print(is_group_member([5, 8, 3], -1))

# 26. Write a Python program to create a histogram from a given list of integers.

def histogram(items):
    for n in items:
        str = ''.join('*' for _ in range(n))
        print(str)

histogram([2, 3, 6, 5])


# 27. Write a Python program to concatenate all elements in a list into a string and return it.


def concatenate_list_data(list):
    return ''.join(str(element) for element in list)


print(concatenate_list_data([1, 5, 12, 2]))


# 28. Write a Python program to print all even numbers from a given numbers list in the same order and stop the printing if any numbers that come after 237 in the sequence.
# Sample numbers list :

# numbers = [
#     386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345,
#     399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217,
#     815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717,
#     958,743, 527
#     ]
numbers = [
    386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345,
    399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217,
    815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717,
    958, 743, 527
]

for x in numbers:
    if x == 237:
        print(x)
        break
    elif x % 2 == 0:
        print(x)


# 29. Write a Python program to print out a set containing all the colors from color_list_1 which are not present in color_list_2.
# Test Data :
# color_list_1 = set(["White", "Black", "Red"])
# color_list_2 = set(["Red", "Green"])
# Expected Output :
# {'Black', 'White'}
color_list_1 = {"White", "Black", "Red"}
color_list_2 = {"Red", "Green"}

print(color_list_1-color_list_2)
dir(color_list_1)

# 30. Write a Python program that will accept the base and height of a triangle and compute the area.
b = int(input("Input the base : "))
h = int(input("Input the height : "))

print("area = ",b * h / 2)

# 31. Write a Python program to compute the greatest common divisor (GCD) of two positive integers.
def gcd(x, y):
    if x % y == 0:
        return y

    return next(
        (k for k in range(int(y / 2), 0, -1) if x % k == 0 and y % k == 0), 1
    )


print(gcd(12, 17))
print(gcd(4, 6))

# 32. Write a Python program to get the least common multiple (LCM) of two positive integers.


def lcm(x, y):
    z = max(x, y)
    while(True):
        if((z % x == 0) and (z % y == 0)):
            lcm = z
            break
        z += 1

    return lcm


print(lcm(4, 6))
print(lcm(15, 17))


# 33. Write a Python program to sum of three given integers. However, if two values are equal sum will be zero.

def sum(x, y, z):
    return 0 if x == y or y == z or x == z else x + y + z


print(sum(2, 1, 2))
print(sum(3, 2, 2))
print(sum(2, 2, 2))
print(sum(1, 2, 3))

# 34. Write a Python program to sum of two given integers. However, if the sum is between 15 to 20 it will return 20.

def sum(x, y):
    sum = x + y
    return 20 if sum in range(15, 20) else sum

print(sum(10, 6))
print(sum(10, 2))
print(sum(10, 12))

# 35. Write a Python program that will return true if the two given integer values are equal or their sum or difference is 5.
def test_number5(x, y):
    return x == y or abs(x-y) == 5 or (x+y) == 5

print(test_number5(7, 2))
print(test_number5(3, 2))
print(test_number5(2, 2))


# 36. Write a Python program to add two objects if both objects are an integer type.
def add_numbers(a, b):
    if not (isinstance(a, int) and isinstance(b, int)):
         raise TypeError("Inputs must be integers")
    return a + b

print(add_numbers(10, 20))


# 37. Write a Python program to display your details like name, age, address in three different lines.
def personal_details():
    name, age = "Simon", 19
    address = "Bangalore, Karnataka, India"
    print(f"Name: {name}\nAge: {age}\nAddress: {address}")

personal_details()

# 38. Write a Python program to solve (x + y) * (x + y).
# Test Data : x = 4, y = 3
# Expected Output : (4 + 3) ^ 2) = 49

x, y = 4, 3
result = x**2 + 2 * x * y + y**2
print(f"({x} + {y}) ^ 2) = {result}")

# 39. Write a Python program to compute the future value of a specified principal amount, rate of interest, and a number of years.
# Test Data : amt = 10000, int = 3.5, years = 7
# Expected Output : 12722.79

amt = 10000
int = 3.5
years = 7

future_value  = amt*((1+(0.01*int)) ** years)
print(round(future_value,2))

# 40. Write a Python program to compute the distance between the points (x1, y1) and (x2, y2).

import math
p1 = [4, 0]
p2 = [6, 6]
distance = math.sqrt( ((p1[0]-p2[0])**2)+((p1[1]-p2[1])**2) )

print(distance)

# 41. Write a Python program to check whether a file exists.
import os.path
open('abc.txt', 'w')
print(os.path.isfile('abc.txt'))


# 42. Write a Python program to determine if a Python shell is executing in 32bit or 64bit mode on OS.
# For 32 bit it will return 32 and for 64 bit it will return 64
import struct
print(struct.calcsize("P") * 8)


# 43. Write a Python program to get OS name, platform and release information.

import platform
import os
print(os.name)
print(platform.system())
print(platform.release())

# 44. Write a Python program to locate Python site-packages.
import site;
print(site.getsitepackages())


# 45. Write a python program to call an external command in Python.
from subprocess import call
call(["ls", "-l"])


# 46. Write a python program to get the path and name of the file that is currently executing.

import os
print("Current File Name : ",os.path.realpath(__file__))

# 47. Write a Python program to find out the number of CPUs using.
import multiprocessing
print(multiprocessing.cpu_count())

# 48. Write a Python program to parse a string to Float or Integer.
n = "246.2458"
print(float(n))
print(int(float(n)))


# 49. Write a Python program to list all files in a directory in Python.
from os import listdir
from os.path import isfile, join
files_list = [f for f in listdir('/home/students') if isfile(join('/home/students', f))]
print(files_list);


# 50. Write a Python program to print without newline or space.
for _ in range(10):
    print('*', end="")
print("\n")


# 51. Write a Python program to determine profiling of Python programs.
# Note: A profile is a set of statistics that describes how often and for how long various parts of the program executed. These statistics can be formatted into reports via the pstats module.

import cProfile
def sum():
    print(1+2)
cProfile.run('sum()')

# 52. Write a Python program to print to stderr.

from __future__ import print_function
import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

eprint("abc", "efg", "xyz", sep="--")

# 53. Write a python program to access environment variables.

import os
# Access all environment variables 
print('*----------------------------------*')
print(os.environ)
print('*----------------------------------*')
# Access a particular environment variable 
print(os.environ['HOME'])
print('*----------------------------------*')
print(os.environ['PATH'])
print('*----------------------------------*')

# 54. Write a Python program to get the current username

import getpass
print(getpass.getuser())

# 55. Write a Python to find local IP addresses using Python's stdlib

import socket
print([l for l in ([ip for ip in socket.gethostbyname_ex(socket.gethostname())[2] 
if not ip.startswith("127.")][:1], [[(s.connect(('8.8.8.8', 53)), 
s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, 
socket.SOCK_DGRAM)]][0][1]]) if l][0][0])

# 56. Write a Python program to get height and width of the console window.

def terminal_size():
    import fcntl, termios, struct
    th, tw, hp, wp = struct.unpack('HHHH',
        fcntl.ioctl(0, termios.TIOCGWINSZ,
        struct.pack('HHHH', 0, 0, 0, 0)))
    return tw, th

print('Number of columns and Rows: ',terminal_size())

# 57. Write a program to get execution time for a Python method.
import time
def sum_of_n_numbers(n):
    start_time = time.time()
    s = 0
    for i in range(1,n+1):
        s = s + i
    end_time = time.time()
    return s,end_time-start_time

n = 5
print("\nTime to sum of 1 to ",n," and required time to calculate is :",sum_of_n_numbers(n))


# 58. Write a python program to find the sum of the first n positive integers.
n = int(input("Input a number: "))
sum_num = (n * (n + 1)) / 2
print(sum_num)


# 59. Write a Python program to convert height (in feet and inches) to centimeters.
print("Input your height: ")
h_ft = int(input("Feet: "))
h_inch = int(input("Inches: "))

h_inch += h_ft * 12
h_cm = round(h_inch * 2.54, 1)

print("Your height is : %d cm." % h_cm)


# 60. Write a Python program to calculate the hypotenuse of a right angled triangle.

from math import sqrt
print("Input lengths of shorter triangle sides:")
a = float(input("a: "))
b = float(input("b: "))

c = sqrt(a**2 + b**2)
print("The length of the hypotenuse is", c )

# 61. Write a Python program to convert the distance (in feet) to inches, yards, and miles.

d_ft = int(input("Input distance in feet: "))
d_inches = d_ft * 12
d_yards = d_ft / 3.0
d_miles = d_ft / 5280.0

print("The distance in inches is %i inches." % d_inches)
print("The distance in yards is %.2f yards." % d_yards)
print("The distance in miles is %.2f miles." % d_miles)

# 62. Write a Python program to convert all units of time into seconds.

days = int(input("Input days: ")) * 3600 * 24
hours = int(input("Input hours: ")) * 3600
minutes = int(input("Input minutes: ")) * 60
seconds = int(input("Input seconds: "))

time = days + hours + minutes + seconds

print("The  amounts of seconds", time)

# 63. Write a Python program to get an absolute file path.

def absolute_file_path(path_fname):
        import os
        return os.path.abspath('path_fname')        
print("Absolute file path: ",absolute_file_path("test.txt"))

# 64. Write a Python program to get file creation and modification date/times.

import os.path, time
print("Last modified: %s" % time.ctime(os.path.getmtime("test.txt")))
print("Created: %s" % time.ctime(os.path.getctime("test.txt")))

# 65. Write a Python program to convert seconds to day, hour, minutes and seconds.

time = float(input("Input time in seconds: "))
day = time // (24 * 3600)
time = time % (24 * 3600)
hour = time // 3600
time %= 3600
minutes = time // 60
time %= 60
seconds = time
print("d:h:m:s-> %d:%d:%d:%d" % (day, hour, minutes, seconds))

# 66. Write a Python program to calculate body mass index.
height = float(input("Input your height in Feet: "))
weight = float(input("Input your weight in Kilogram: "))
print("Your body mass index is: ", round(weight / (height * height), 2))


# 67. Write a Python program to convert pressure in kilopascals to pounds per square inch, a millimeter of mercury (mmHg) and atmosphere pressure.
kpa = float(input("Input pressure in in kilopascals> "))
psi = kpa / 6.89475729
mmhg = kpa * 760 / 101.325
atm = kpa / 101.325
print("The pressure in pounds per square inch: %.2f psi"  % (psi))
print("The pressure in millimeter of mercury: %.2f mmHg" % (mmhg))
print("Atmosphere pressure: %.2f atm." % (atm))


# 68. Write a Python program to calculate the sum of the digits in an integer.

num = int(input("Input a four digit numbers: "))
x  = num //1000
x1 = (num - x*1000)//100
x2 = (num - x*1000 - x1*100)//10
x3 = num - x*1000 - x1*100 - x2*10
print("The sum of digits in the number is", x+x1+x2+x3)

# 69. Write a Python program to sort three integers without using conditional statements and loops.

x = int(input("Input first number: "))
y = int(input("Input second number: "))
z = int(input("Input third number: "))

a1 = min(x, y, z)
a3 = max(x, y, z)
a2 = (x + y + z) - a1 - a3
print("Numbers in sorted order: ", a1, a2, a3)

# 70. Write a Python program to sort files by date.

import glob
import os

files = glob.glob("*.txt")
files.sort(key=os.path.getmtime)
print("\n".join(files))

# 71. Write a Python program to get a directory listing, sorted by creation date.

from stat import S_ISREG, ST_CTIME, ST_MODE
import os, sys, time

#Relative or absolute path to the directory
dir_path = sys.argv[1] if len(sys.argv) == 2 else r'.'

#all entries in the directory w/ stats
data = (os.path.join(dir_path, fn) for fn in os.listdir(dir_path))
data = ((os.stat(path), path) for path in data)

# regular files, insert creation date
data = ((stat[ST_CTIME], path)
           for stat, path in data if S_ISREG(stat[ST_MODE]))

for cdate, path in sorted(data):
    print(time.ctime(cdate), os.path.basename(path))
	
# 72. Write a Python program to get the details of math module.

# Imports the math module
import math            
#Sets everything to a list of math module
math_ls = dir(math) # 
print(math_ls)

# 73. Write a Python program to calculate midpoints of a line.

print('\nCalculate the midpoint of a line :')

x1 = float(input('The value of x (the first endpoint) '))
y1 = float(input('The value of y (the first endpoint) '))

x2 = float(input('The value of x (the first endpoint) '))
y2 = float(input('The value of y (the first endpoint) '))

x_m_point = (x1 + x2)/2
y_m_point = (y1 + y2)/2
print();
print("The midpoint of line is :")
print( "The midpoint's x value is: ",x_m_point)
print( "The midpoint's y value is: ",y_m_point)
print();

# 74. Write a Python program to hash a word.

soundex=[0,1,2,3,0,1,2,0,0,2,2,4,5,5,0,1,2,6,2,3,0,1,0,2,0,2]
 
word=input("Input the word be hashed: ")
 
word=word.upper()
 
coded=word[0]
 
for a in word[1:len(word)]:
    i=65-ord(a)
    coded=coded+str(soundex[i])
print() 
print("The coded word is: "+coded)
print()

# 75. Write a Python program to get the copyright information.

import sys
print("\nPython Copyright Information")
print(sys.copyright)
print()

# 76. Write a Python program to get the command-line arguments (name of the script, the number of arguments, arguments) passed to a script.

import sys
print("This is the name/path of the script:"),sys.argv[0]
print("Number of arguments:",len(sys.argv))
print("Argument List:",str(sys.argv))

# 77. Write a Python program to test whether the system is a big-endian platform or little-endian platform.

import sys
print()
if sys.byteorder == "little":
    #intel, alpha
    print("Little-endian platform.")
else:
    #motorola, sparc
    print("Big-endian platform.")
print()

# 78. Write a Python program to find the available built-in modules.

import sys
import textwrap
module_name = ', '.join(sorted(sys.builtin_module_names))
print(textwrap.fill(module_name, width=70))

# 79. Write a Python program to get the size of an object in bytes.

import sys
str1 = "one"
str2 = "four"
str3 = "three"
print()
print("Memory size of '"+str1+"' = "+str(sys.getsizeof(str1))+ " bytes")
print("Memory size of '"+str2+"' = "+str(sys.getsizeof(str2))+ " bytes")
print("Memory size of '"+str3+"' = "+str(sys.getsizeof(str3))+ " bytes")
print()

# 80. Write a Python program to get the current value of the recursion limit.
import sys
print()
print("Current value of the recursion limit:")
print(sys.getrecursionlimit())
print()


# 81. Write a Python program to concatenate N strings.

list_of_colors = ['Red', 'White', 'Black']  
colors = '-'.join(list_of_colors)
print()
print("All Colors: "+colors)
print()

# 82. Write a Python program to calculate the sum over a container.

s = sum([10,20,30])
print("\nSum of the container: ", s)
print()

# 83. Write a Python program to test whether all numbers of a list is greater than a certain number.

num = [2,3,4]
print()
print(all(x > 1 for x in num))
print(all(x > 4 for x in num))
print()

# 84. Write a Python program to count the number occurrence of a specific character in a string.

s = "The quick brown fox jumps over the lazy dog."
print()
print(s.count("q"))
print()

# 85. Write a Python program to check whether a file path is a file or a directory.

import os  
path="abc.txt"  
if os.path.isdir(path):  
    print("\nIt is a directory")  
elif os.path.isfile(path):  
    print("\nIt is a normal file")  
else:  
    print("It is a special file (socket, FIFO, device file)" )
print()

# 86. Write a Python program to get the ASCII value of a character.

print()
print(ord('a'))
print(ord('A'))
print(ord('1'))
print(ord('@'))
print()

# 87. Write a Python program to get the size of a file.

import os
file_size = os.path.getsize("abc.txt")
print("\nThe size of abc.txt is :",file_size,"Bytes")
print()

# 88. Given variables x=30 and y=20, write a Python program to print "30+20=50".

x = 30
y = 20
print("\n%d+%d=%d" % (x, y, x+y))
print()

# 89. Write a Python program to perform an action if a condition is true.
# Given a variable name, if the value is 1, display the string "First day of a Month!" and do nothing if the value is not equal.

n=1
if n == 1:
    print("\nFirst day of a month")
print()

# 90. Write a Python program to create a copy of its own source code.

print()
print((lambda str='print(lambda str=%r: (str %% str))()': (str % str))())
print()

# 91. Write a Python program to swap two variables.

a = 30
b = 20
print("\nBefore swap a = %d and b = %d" %(a, b))
a, b = b, a
print("\nAfter swaping a = %d and b = %d" %(a, b))
print()

# 92. Write a Python program to define a string containing special characters in various forms.

print()
print("\#{'}${\"}@/")
print("\#{'}${"'"'"}@/")
print(r"""\#{'}${"}@/""")
print('\#{\'}${"}@/')
print('\#{'"'"'}${"}@/')
print(r'''\#{'}${"}@/''')
print()

# 93. Write a Python program to get the identity of an object.

obj1 = object()
obj1_address = id(obj1)
print()
print(obj1_address)
print()

# 94. Write a Python program to convert a byte string to a list of integers.

x = b'Abc'
print()
print(list(x))
print()

# 95. Write a Python program to check whether a string is numeric.
str = 'a123'
#str = '123'
try:
    i = float(str)
except (ValueError, TypeError):
    print('\nNot numeric')
print()


# 96. Write a Python program to print the current call stack.
import traceback
print()
def f1():return abc()
def abc():traceback.print_stack()
f1()
print()


# 97. Write a Python program to list the special variables used within the language.
s_var_names = sorted((set(globals().keys()) | set(__builtins__.__dict__.keys())) - set('_ names i'.split()))
print()
print( '\n'.join(' '.join(s_var_names[i:i+8]) for i in range(0, len(s_var_names), 8)) )
print()


# 98. Write a Python program to get the system time.
# Note : The system time is important for debugging, network information, random number seeds, or something as simple as program performance.
import time
print()
print(time.ctime())
print()

# 99. Write a Python program to clear the screen or terminal.
import os
import time
# for windows
# os.system('cls')
os.system("ls")
time.sleep(2)
# Ubuntu version 10.10
os.system('clear')


# 100. Write a Python program to get the name of the host on which the routine is running.

import socket
host_name = socket.gethostname()
print()
print("Host name:", host_name)
print()

# 101. Write a Python program to access and print a URL's content to the console.

from http.client import HTTPConnection
conn = HTTPConnection("example.com")
conn.request("GET", "/")  
result = conn.getresponse()
# retrieves the entire contents.  
contents = result.read() 
print(contents)

# 102. Write a Python program to get system command output.
import subprocess
# file and directory listing
returned_text = subprocess.check_output("dir", shell=True, universal_newlines=True)
print("dir command to list file and directory")
print(returned_text)


# 103. Write a Python program to extract the filename from a given path.

import os
print()
print(os.path.basename('/users/system1/student1/homework-1.py'))
print()

# 104. Write a Python program to get the effective group id, effective user id, real group id, a list of supplemental group ids associated with the current process.
# Note: Availability: Unix.

import os
print("\nEffective group id: ",os.getegid())
print("Effective user id: ",os.geteuid())
print("Real group id: ",os.getgid())
print("List of supplemental group ids: ",os.getgroups())
print()

# 105. Write a Python program to get the users environment.

import os
print()
print(os.environ)
print()

# 106. Write a Python program to divide a path on the extension separator.

import os.path
for path in [ 'test.txt', 'filename', '/user/system/test.txt', '/', '' ]:
    print('"%s" :' % path, os.path.splitext(path))
	
# 107. Write a Python program to retrieve file properties.
import os.path
import time

print('File         :', __file__)
print('Access time  :', time.ctime(os.path.getatime(__file__)))
print('Modified time:', time.ctime(os.path.getmtime(__file__)))
print('Change time  :', time.ctime(os.path.getctime(__file__)))
print('Size         :', os.path.getsize(__file__))


# 108. Write a Python program to find path refers to a file or directory when you encounter a path name.

import os.path

for file in [ __file__, os.path.dirname(__file__), '/', './broken_link']:
    print('File        :', file)
    print('Absolute    :', os.path.isabs(file))
    print('Is File?    :', os.path.isfile(file))
    print('Is Dir?     :', os.path.isdir(file))
    print('Is Link?    :', os.path.islink(file))
    print('Exists?     :', os.path.exists(file))
    print('Link Exists?:', os.path.lexists(file))
	
# 109. Write a Python program to check if a number is positive, negative or zero.

num = float(input("Input a number: "))
if num > 0:
   print("It is positive number")
elif num == 0:
   print("It is Zero")
else:
   print("It is a negative number")
   
# 110. Write a Python program to get numbers divisible by fifteen from a list using an anonymous function.
num_list = [45, 55, 60, 37, 100, 105, 220]
# use anonymous function to filter
result = list(filter(lambda x: (x % 15 == 0), num_list))
print("Numbers divisible by 15 are",result)


# 111. Write a Python program to make file lists from current directory using a wildcard.

import glob
file_list = glob.glob('*.*')
print(file_list)

# 112. Write a Python program to remove the first item from a specified list.
color = ["Red", "Black", "Green", "White", "Orange"]
print("\nOriginal Color: ",color)
del color[0]
print("After removing the first color: ",color)
print()


# 113. Write a Python program to input a number, if it is not a number generate an error message.
while True:
    try:
        a = int(input("Input a number: "))
        break
    except ValueError:
        print("\nThis is not a number. Try again...")
        print()
		

# 114. Write a Python program to filter the positive numbers from a list.

nums = [34, 1, 0, -23]
print("Original numbers in the list: ",nums)
new_nums = list(filter(lambda x: x >0, nums))
print("Positive numbers in the list: ",new_nums)

# 115. Write a Python program to compute the product of a list of integers (without using for loop).

from functools import reduce
nums = [10, 20, 30,]
nums_product = reduce( (lambda x, y: x * y), nums)
print("Product of the numbers : ",nums_product)

# 116. Write a Python program to print Unicode characters.

str = u'\u0050\u0079\u0074\u0068\u006f\u006e \u0045\u0078\u0065\u0072\u0063\u0069\u0073\u0065\u0073 \u002d \u0077\u0033\u0072\u0065\u0073\u006f\u0075\u0072\u0063\u0065'
print()
print(str)
print()

# 117. Write a Python program to prove that two string variables of same value point same memory location.

str1 = "Python"
str2 = "Python"
 
print("\nMemory location of str1 =", hex(id(str1)))
print("Memory location of str2 =", hex(id(str2)))
print()

# 118. Write a Python program to create a bytearray from a list.

print()
nums = [10, 20, 56, 35, 17, 99]
# Create bytearray from list of integers.
values = bytearray(nums)
for x in values: print(x)
print()

# 119. Write a Python program to display a floating number in specified numbers.

order_amt = 212.374
print('\nThe total order amount comes to %f' % order_amt)
print('The total order amount comes to %.2f' % order_amt)
print()

# 120. Write a Python program to format a specified string to limit the number of characters to 6.
str_num = "1234567890"
print()
print('%.6s' % str_num)
print()


# 121. Write a Python program to determine whether variable is defined or not.
try:
  x = 1
except NameError:
  print("Variable is not defined....!")
else:
  print("Variable is defined.")
try:
  y
except NameError:
  print("Variable is not defined....!")
else:
  print("Variable is defined.")
  

# 122. Write a Python program to empty a variable without destroying it.

# Sample data: n=20
# d = {"x":200}
# Expected Output : 0
# {}
n = 20
d = {"x":200}
l = [1,3,5]
t= (5,7,8)
print(type(n)())
print(type(d)())
print(type(l)())
print(type(t)()) 


# 123. Write a Python program to determine the largest and smallest integers, longs, floats.

import sys
print("Float value information: ",sys.float_info)
print("\nInteger value information: ",sys.int_info)
print("\nMaximum size of an integer: ",sys.maxsize) 

# 124. Write a Python program to check whether multiple variables have the same value.
x = 20
y = 20
z = 20
if x == y == z == 20:
    print("All variables have same value!")  
	

# 125. Write a Python program to sum of all counts in a collections?
import collections
num = [2,2,4,6,6,8,6,10,4]
print(sum(collections.Counter(num).values()))


# 126. Write a Python program to get the actual module object for a given object.

from inspect import getmodule
from math import sqrt
print(getmodule(sqrt))

# 127. Write a Python program to check whether an integer fits in 64 bits.

int_val = 30
if int_val.bit_length() <= 63:
    print((-2 ** 63).bit_length())
    print((2 ** 63).bit_length())
	
# 128. Write a Python program to check whether lowercase letters exist in a string.
str1 = 'A8238i823acdeOUEI'
print(any(c.islower() for c in str1))


# 129. Write a Python program to add trailing and leading zeroes to a string.
str1='122.22'
print("Original String: ",str1)
print("\nAdded trailing zeros:")
str1 = str1.ljust(8, '0')
print(str1)
str1 = str1.ljust(10, '0')
print(str1)
print("\nAdded leading zeros:")
str1='122.22'
str1 = str1.rjust(8, '0')
print(str1)
str1 = str1.rjust(10, '0')
print(str1)


# 130. Write a Python program to use double quotes to display strings.

import json
print(json.dumps({'Alex': 1, 'Suresh': 2, 'Agnessa': 3}))

# 131. Write a Python program to split a variable length string into variables.
var_list = ['a', 'b', 'c']
x, y, z = (var_list + [None] * 3)[:3]
print(x, y, z)
var_list = [100, 20.25]
x, y = (var_list + [None] * 2)[:2]
print(x, y)


# 132. Write a Python program to list home directory without absolute path.
import os.path
print(os.path.expanduser('~'))


# 133. Write a Python program to calculate the time runs (difference between start and current time) of a program.
from timeit import default_timer
def timer(n):
    start = default_timer()
    # some code here
    for row in range(0,n):
        print(row)
    print(default_timer() - start)

timer(5)
timer(15)


# 134. Write a Python program to input two integers in a single line.
print("Input the value of x & y")
x, y = map(int, input().split())
print("The value of x & y are: ",x,y)


# 135. Write a Python program to print a variable without spaces between values.
# Sample value : x =30
# Expected output : Value of x is "30"

x = 30
print('Value of x is "{}"'.format(x))

# 136. Write a Python program to find files and skip directories of a given directory.

import os
print([f for f in os.listdir('/home/students') if os.path.isfile(os.path.join('/home/students', f))])

# 137. Write a Python program to extract single key-value pair of a dictionary in variables.

d = {'Red': 'Green'}
(c1, c2), = d.items()
print(c1)
print(c2)

# 138. Write a Python program to convert true to 1 and false to 0.

x = 'true'
x = int(x == 'true')
print(x)
x = 'abcd'
x = int(x == 'true')
print(x)

# 139. Write a Python program to valid a IP address.

import socket
addr = '127.0.0.2561'
try:
    socket.inet_aton(addr)
    print("Valid IP")
except socket.error:
    print("Invalid IP")
	
# 140. Write a Python program to convert an integer to binary keep leading zeros.
# Sample data : x=12
# Expected output : 00001100
# 0000001100

x = 12
print(format(x, '08b'))
print(format(x, '010b'))

# 141. Write a python program to convert decimal to hexadecimal.
# Sample decimal number: 30, 4
# Expected output: 1e, 04
x = 30
print(format(x, '02x'))
x = 4
print(format(x, '02x'))


# 142. Write a Python program to find the operating system name, platform and platform release date.
# Operating system name:
# posix
# Platform name:
# Linux
# Platform release:
# 4.4.0-47-generic

import os, platform
print("Operating system name:")
print(os.name)
print("Platform name:")
print(platform.system())
print("Platform release:")
print(platform.release())

# 143. Write a Python program to determine if the python shell is executing in 32bit or 64bit mode on operating system.

import struct
print(struct.calcsize("P") * 8)

# 144. Write a Python program to check whether variable is of integer or string.

print(isinstance(25,int) or isinstance(25,str))
print(isinstance([25],int) or isinstance([25],str))
print(isinstance("25",int) or isinstance("25",str))

# 145. Write a Python program to test if a variable is a list or tuple or a set.

#x = ['a', 'b', 'c', 'd']
#x = {'a', 'b', 'c', 'd'}
x = ('tuple', False, 3.2, 1)
if type(x) is list:
    print('x is a list')
elif type(x) is set:
    print('x is a set')
elif type(x) is tuple:
    print('x is a tuple')    
else:
    print('Neither a list nor a set nor a tuple.')

# 146. Write a Python program to find the location of Python module sources.

import sys
print("\nList of directories in sys module:")
print(sys.path)
print("\nList of directories in os module:")
import os
print(os.path)

# 147. Write a Python function to check whether a number is divisible by another number. Accept two integers values form the user.
def multiple(m, n):
	return True if m % n == 0 else False

print(multiple(20, 5))
print(multiple(7, 2))


# 148. Write a Python function to find the maximum and minimum numbers from a sequence of numbers.
# Note: Do not use built-in functions.

def max_min(data):
  l = data[0]
  s = data[0]
  for num in data:
    if num> l:
      l = num
    elif num< s:
        s = num
  return l, s

print(max_min([0, 10, 15, 40, -5, 42, 17, 28, 75]))

# 149. Write a Python function that takes a positive integer and returns the sum of the cube of all the positive integers smaller than the specified number.

def sum_of_cubes(n):
  n -= 1
  total = 0
  while n > 0:
    total += n * n * n
    n -= 1
  return total
print("Sum of cubes: ",sum_of_cubes(3))

# 150. Write a Python function to find a distinct pair of numbers whose product is odd from a sequence of integer values.
def odd_product(nums):
  for i in range(len(nums)):
    for j in range(len(nums)):
      if  i != j:
        product = nums[i] * nums[j]
        if product & 1:
          return True
          return False
          
dt1 = [2, 4, 6, 8]
dt2 = [1, 6, 4, 7, 8]
print(dt1, odd_product(dt1))
print(dt2, odd_product(dt2))




Basic Exercises II.py '''
PYTHON BASIC EXCERCISES PART II SOLUTIONS (VIA W3RESOURCE PYTHON)
AUTHORED BY: ATHARVA "HIGHNESS_ATHARVA" SHAH
'''
import os
from multiprocessing import Process
import requests
import string
from collections import Counter
from itertools import chain, compress
from bisect import bisect_right
import sys
from datetime import date
import math
import re
from collections import deque
from functools import partial
import itertools
import platform as pl
import pkg_resources
from urllib.request import urlopen
from bs4 import BeautifulSoup as soup
import bs4
import pprint
import collections
import random


# 1. function that takes a sequence of numbers and determines whether all the numbers are different from each other

def test_distinct(data):
    return len(data) == len(set(data))


print(test_distinct([1, 5, 7, 9]))
print(test_distinct([2, 4, 5, 5, 7, 9]))

# 2. create all possible strings by using 'a', 'e', 'i', 'o', 'u'. Use the characters exactly once

char_list = ['a', 'e', 'i', 'o', 'u']
random.shuffle(char_list)
print(''.join(char_list))

# 3. remove and print every third number from a list of numbers until the list becomes empty.

def remove_nums(int_list):
    # list starts with 0 index
    position = 3 - 1
    idx = 0
    len_list = (len(int_list))
    while len_list > 0:
        idx = (position+idx) % len_list
        print(int_list.pop(idx))
        len_list -= 1


nums = [10, 20, 30, 40, 50, 60, 70, 80, 90]
remove_nums(nums)

# 4. (THREE SUM)find unique triplets whose three elements gives the sum of zero from an array of n integers.

def three_sum(nums):
    result = []
    nums.sort()
    for i in range(len(nums)-2):
        if i > 0 and nums[i] == nums[i-1]:
            continue
        l, r = i+1, len(nums)-1
        while l < r:
            s = nums[i] + nums[l] + nums[r]
            if s > 0:
                r -= 1
            elif s < 0:
                l += 1
            else:
                # found three sum
                result.append((nums[i], nums[l], nums[r]))
                # remove duplicates
                while l < r and nums[l] == nums[l+1]:
                    l += 1
                    while l < r and nums[r] == nums[r-1]:
                        r -= 1
                        l += 1
                        r -= 1
                    return result


x = [1, -6, 4, 2, -1, 2, 0, -2, 0]
print(three_sum(x))


for num in range(1000):
    num = str(num).zfill(3)
print(num)
numbers = [num]
# 6. print a long text, convert the string to a list and print all the words and their frequencies

string_words = '''United States Declaration of Independence
From Wikipedia, the free encyclopedia
The United States Declaration of Independence is the statement
adopted by the Second Continental Congress meeting at the Pennsylvania State
House (Independence Hall) in Philadelphia on July 4, 1776, which announced
that the thirteen American colonies, then at war with the Kingdom of Great
Britain, regarded themselves as thirteen independent sovereign states, no longer
under British rule. These states would found a new nation – the United States of
America. John Adams was a leader in pushing for independence, which was passed
on July 2 with no opposing vote cast. A committee of five had already drafted the
formal declaration, to be ready when Congress voted on independence.
'''
word_list = string_words.split()
word_freq = [word_list.count(n) for n in word_list]

print(f"String:\n {string_words} \n")
print(f"List:\n {word_list} \n")
print(f"Pairs (Words and Frequencies:\n {list(zip(word_list, word_freq))}")

# 7. count the number of each character of a given text of a text file

file_input = input('File Name: ')
with open(file_input, 'r') as info:
    count = collections.Counter(info.read().upper())
    value = pprint.pformat(count)
print(value)


# 8. get the top stories from Google news.

news_url = "https://news.google.com/news/rss"
Client = urlopen(news_url)
xml_page = Client.read()
Client.close()

soup_page = soup(xml_page, "xml")
news_list = soup_page.findAll("item")
# Print news title, url and publish date
for news in news_list:
    print(news.title.text)
    print(news.link.text)
    print(news.pubDate.text)
    print("-"*60)


# 9. get a list of locally installed Python modules.

installed_packages = pkg_resources.working_set
installed_packages_list = sorted(
    [f"{i.key}=={i.version}" for i in installed_packages]
)
for m in installed_packages_list:
    print(m)


# 10. display some information about the OS where the script is running

os_profile = [
    'architecture',
    'linux_distribution',
    'mac_ver',
    'machine',
    'node',
    'platform',
    'processor',
    'python_build',
    'python_compiler',
    'python_version',
    'release',
    'system',
    'uname',
    'version',
]
for key in os_profile:
    if hasattr(pl, key):
        print(f"{key}: {str(getattr(pl, key)())}")

# 11. check the sum of three elements (each from an array) from three arrays is equal to a target value. Print all those three-element combinations.

X = [10, 20, 20, 20]
Y = [10, 20, 30, 40]
Z = [10, 30, 40, 20]
T = 70
   
   
def check_sum_array(N, *nums):
    return (True, nums) if sum(nums) == N else (False, nums)


pro = itertools.product(X, Y, Z)
func = partial(check_sum_array, T)
sums = list(itertools.starmap(func, pro))

result = set()
for s in sums:
    if s[0] == True and s[1] not in result:
        result.add(s[1])
        print(result)


# 12. create all possible permutations from a given collection of distinct numbers



def permute(nums):
    result_perms = [[]]
    for n in nums:
        new_perms = []
        for perm in result_perms:
            for i in range(len(perm)+1):
                new_perms.append(perm[:i] + [n] + perm[i:])
                result_perms = new_perms
    return result_perms


my_nums = [1, 2, 3]
print("Original Cofllection: ", my_nums)
print("Collection of distinct numbers:\n", permute(my_nums))

# 13. get all possible two digit letter combinations from a digit (1 to 9) string.



def letter_combinations(digits):
    if digits == "":
        return []
    string_maps = {
        "1": "abc",
        "2": "def",
        "3": "ghi",
        "4": "jkl",
        "5": "mno",
        "6": "pqrs",
        "7": "tuv",
        "8": "wxy",
        "9": "z"
    }
    result = [""]
    for num in digits:
        temp = []
        for an in result:
            temp.extend(an + char for char in string_maps[num])
        result = temp
    return result


digit_string = "47"
print(letter_combinations(digit_string))
digit_string = "29"
print(letter_combinations(digit_string))

# 14. add two positive integers without using the '+' operator. Go to the editor
# Note: Use bit wise operations to add two numbers.



def add_without_plus_operator(a, b):
    while b != 0:
        data = a & b
        a = a ^ b
        b = data << 1
    return a


print(add_without_plus_operator(2, 10))
print(add_without_plus_operator(-20, 10))
print(add_without_plus_operator(-10, -20))

# 15. check the priority of the four operators (+, -, *, /).

__operators__ = "+-/*"
__parenthesis__ = "()"
__priority__ = {
    '+': 0,
    '-': 0,
    '*': 1,
    '/': 1,
}


def test_higher_priority(operator1, operator2):
    return __priority__[operator1] >= __priority__[operator2]


print(test_higher_priority('*', '-'))
print(test_higher_priority('+', '-'))
print(test_higher_priority('+', '*'))
print(test_higher_priority('+', '/'))
print(test_higher_priority('*', '/'))

# 16. get the third side of right angled triangle from two given sides.



def pythagoras(opposite_side, adjacent_side, hypotenuse):
    if opposite_side == "x":
        return f"Opposite = {str((hypotenuse**2 - adjacent_side**2)**0.5)}"
    elif adjacent_side == "x":
        return f"Adjacent = {str((hypotenuse**2 - opposite_side**2)**0.5)}"
    elif hypotenuse == "x":
        return f"Hypotenuse = {str((opposite_side**2 + adjacent_side**2)**0.5)}"
    else:
        return "You know the answer!"


print(pythagoras(3, 4, 'x'))
print(pythagoras(3, 'x', 5))
print(pythagoras('x', 4, 5))
print(pythagoras(3, 4, 5))

# 17. get all strobogrammatic numbers that are of length n. Go to the editor
# A strobogrammatic number is a number whose numeral is rotationally symmetric, so that it appears the same when rotated 180 degrees. In other words, the numeral looks the same right-side up and upside down (e.g., 69, 96, 1001).
# For example,
# Given n = 2, return ["11", "69", "88", "96"].
# Given n = 3, return ['818', '111', '916', '619', '808', '101', '906', '609', '888', '181', '986', '689']



def gen_strobogrammatic(n):
    """
    :type n: int
    :rtype: List[str]
    """
    return helper(n, n)


def helper(n, length):
    if n == 0:
        return [""]
    if n == 1:
        return ["1", "0", "8"]
    middles = helper(n-2, length)
    result = []
    for middle in middles:
        if n != length:
            result.append(f"0{middle}0")
        result.extend((f"8{middle}8", f"1{middle}1", f"9{middle}6", f"6{middle}9"))
    return result


print("n = 2: \n", gen_strobogrammatic(2))
print("n = 3: \n", gen_strobogrammatic(3))
print("n = 4: \n", gen_strobogrammatic(4))

# 18. Find the mean, median, mode of N Numbers without using Stats module

'''
MEAN
'''
n_num = [1, 2, 3, 4, 5]
n = len(n_num)
get_sum = sum(n_num)
mean = get_sum / n
print(f"Mean / Average is: {str(mean)}")

'''
MODE
'''
# list of elements to calculate mode
n_num = [1, 2, 3, 4, 5, 5]
n = len(n_num)

data = Counter(n_num)
get_mode = dict(data)
mode = [k for k, v in get_mode.items() if v == max(list(data.values()))]

if len(mode) == n:
    get_mode = "No mode found"
else:
    get_mode = "Mode is / are: " + ', '.join(map(str, mode))
print(get_mode)

'''
MEDIAN
'''
n_num = [1, 2, 3, 4, 5]
n = len(n_num)
n_num.sort()

if n % 2 == 0:
    median1 = n_num[n//2]
    median2 = n_num[n//2 - 1]
    median = (median1 + median2)/2
else:
    median = n_num[n//2]
print(f"Median is: {str(median)}")


# 19. find the value of n where n degrees of number 2 are written sequentially in a line without spaces



def ndegrees(num):
    ans = True
    n, tempn, i = 2, 2, 2
    while ans:
        if str(tempn) in num:
            i += 1
            tempn = pow(n, i)
        else:
            ans = False
    return i-1


print(ndegrees("2481632"))
print(ndegrees("248163264"))

# 20. find the number of zeros at the end of a factorial of a given positive number.



def factendzero(n):
    x = n // 5
    y = x
    while x > 0:
        x /= 5
        y += x
    return y


print(factendzero(5))
print(factendzero(12))
print(factendzero(100))

# 21. find the number of notes (Sample of notes: 10, 20, 50, 100, 200 and 500 ) against a given amount



def no_notes(a):
    Q = [500, 200, 100, 50, 20, 10]
    x = 0
    for i in range(6):
        q = Q[i]
        x += int(a / q)
        a = int(a % q)
    if a > 0:
        x = -1
    return x


print(no_notes(880))
print(no_notes(1000))

# 22. create a sequence where the first four members of the sequence are equal to one, and each successive term of the sequence is equal to the sum of the four previous ones. Find the Nth member of the sequence


def new_seq(n):
    if n in [1, 2, 3, 4]:
        return 1
    return new_seq(n-1) + new_seq(n-2) + new_seq(n-3) + new_seq(n-4)


print(new_seq(5))
print(new_seq(6))
print(new_seq(7))

# 23. Accept a positive number and subtract from this number the sum of its digits
num = int(input('Please enter a multi-digit number: '))
if num > 0:
    num -= sum(int(x) for x in str(num))
    print(num)


# 24. Find the number of divisors of a given integer is even or odd
def divisor(n):
    for i in range(n):
        x = len([i for i in range(1, n+1) if not n % i])
    return x


print(divisor(15))
print(divisor(12))
print(divisor(9))
print(divisor(6))
print(divisor(3))

# 25.  Find the digits which are absent in a given mobile number


def absent_digits(n):
    all_nums = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    n = {int(i) for i in n}
    n = n.symmetric_difference(all_nums)
    return sorted(n)


print(absent_digits([9, 8, 3, 2, 2, 0, 9, 7, 6, 3]))

# 26. Compute the summation of the absolute difference of all distinct pairs in an given array


def dist_sum(arr):
    return sum(i*arr[i]-(len(arr)-i-1)*arr[i] for i in range(len(arr)))


# 3-1=2, 2-1=1, 3-2=1 so 2+1+1=4 which is absolute difference.
print(dist_sum([1, 2, 3]))
# 5-1=4, 4-1=3, 5-4=1 so 4+3+1=8 which is absolute difference.
print(dist_sum([1, 4, 5]))

# 27.  Find the type of the progression and the next successive member of a given three successive members of a sequence
# For this solution we do not include HP, only AP and GP


def ap_gp_sequence(arr):
    if arr[0] == arr[1] == arr[2] == 0:
        return "Wrong Numbers"
    if arr[1]-arr[0] == arr[2]-arr[1]:
        n = 2*arr[2]-arr[1]
        return "AP sequence, "+'Next number of the sequence: '+str(n)
    else:
        n = arr[2]**2/arr[1]
        return "GP sequence, " + 'Next number of the sequence:  '+str(n)


print(ap_gp_sequence([1, 2, 3]))
print(ap_gp_sequence([2, 6, 18]))
print(ap_gp_sequence([0, 0, 0]))

# 28.  print the length of the series and the series from the given 3rd term, 3rd last term and the sum of a series.
tn = int(input("Input third term of the series:"))
tltn = int(input("Input 3rd last term:"))
s_sum = int(input("Sum of the series:"))
n = int(2*s_sum/(tn+tltn))
print("Length of the series: ", n)


d = (s_sum-3*tn)//6 if n == 5 else (tltn-tn)/(n-5)
a = tn-2*d
j = 0
print("Series:")
for _ in range(n-1):
    print(int(a), end=" ")
    a += d
print(int(a), end=" ")

# 29. Find common divisors between two numbers in a given pair.


def ngcd(x, y):
    i = 1
    while(i <= x and i <= y):
        if(x % i == 0 and y % i == 0):
            gcd = i
        i += 1
    return gcd


def num_comm_div(x, y):
    n = ngcd(x, y)
    result = 0
    z = int(n**0.5)
    for i in range(1, z + 1):
        if(n % i == 0):
            result += 2
            if(i == n/i):
                result -= 1
    return result


print("Number of common divisors: ", num_comm_div(2, 4))
print("Number of common divisors: ", num_comm_div(2, 8))
print("Number of common divisors: ", num_comm_div(12, 24))

# 30. reverse the digits of a given number and add it to the original, If the sum is not a palindrome repeat this procedure.
# Note: A palindrome is a word, number, or other sequence of characters which reads the same backward as forward, such as madam or racecar.
# P.S: Master solutio provided by Website is incorrect. I have offered a much cleaner and efficient solution.


def revadder(n):
    number = str(n)
    number = list(number)
    number.sort(reverse=True)
    return int(''.join(number)) + n


print(revadder(45))

# 31.  count the number of carry operations for each of a set of addition problems


def carry_number(x, y):
    ctr = 0
    if(x == 0 and y == 0):
        return 0
    z = 0
    for _ in reversed(range(10)):
        z = x % 10 + y % 10 + z
        z = 1 if z > 9 else 0
        ctr += z
        x //= 10
        y //= 10

    return "No carry operation." if ctr == 0 else ctr


print(carry_number(786, 457))
print(carry_number(5, 6))

# 32. python program to find heights of the top three building in descending order from eight given buildings
print("Input the heights of eight buildings:")
l = [int(input()) for _ in range(8)]
print("Heights of the top three buildings:")
l.sort(reverse=True)
print(l[:3], end='\n')

# 33.  compute the digit number of sum of two given integers
print("Input two integers(a b): ")
a, b = map(int, input().split(" "))
print("Number of digit of a and b.:")
print(len(str(a+b)))

# 34. check whether three given lengths (integers) of three sides form a right triangle. Print "Yes" if the given sides form a right triangle otherwise print "No"
print("Input three integers(sides of a triangle)")
int_num = list(map(int, input().split()))
x, y, z = sorted(int_num)
if x**2+y**2 == z**2:
    print('Yes')
else:
    print('No')

print("Input the value of a, b, c, d, e, f:")
a, b, c, d, e, f = map(float, input().split())
n = a*e - b*d
print("Values of x and y:")
if n != 0:
    x = (c*e - b*f) / n
    y = (a*f - c*d) / n
    print('{:.3f} {:.3f}'.format(x+0, y+0))

# 36. compute the amount of the debt in n months. The borrowing amount is $100,000 and the loan adds 5% interest of the debt and rounds it to the nearest 1,000 above month by month.
#Interest is fixed @5% PA
x = int(input("Amount? "))
period = int(input("Period? "))
for _ in range(period):
    x = math.trunc(x*.05+x)
    x = x if x % 1000 == 0 else (1000-(x % 1000))+x
print("Amount of Debt: ", x)

# 37. program which reads an integer n and find the number of combinations of a,b,c and d (0 <= a,b,c,d <= 9) where (a + b + c + d) will be equal to n.
print("Input the number(n):")
n = int(input())
result = sum(
    (0 <= n - (i + j + k) <= 9)
    for i, j, k in itertools.product(range(10), range(10), range(10))
)
print("Number of combinations:", result)

# 38. print the number of prime numbers which are less than or equal to a given integer.


def is_prime(n):
    for i in range(2, int(n//2)+1):
        return n % i != 0


n = int(input('Input an integer: '))
list_primes = [j for j in range(2, n+1) if is_prime(j)]
print("Number of prime numbers which are less than or equal to n.:",
      len(list_primes))

# 39. compute the radius and the central coordinate (x, y) of a circle which is constructed by three given points on the plane surface.
print("Input three coordinate of the circle:")
x1, y1, x2, y2, x3, y3 = map(float, input().split())
c = (x1-x2)**2 + (y1-y2)**2
a = (x2-x3)**2 + (y2-y3)**2
b = (x3-x1)**2 + (y3-y1)**2
s = 2*(a*b + b*c + c*a) - (a*a + b*b + c*c)
px = (a*(b+c-a)*x1 + b*(c+a-b)*x2 + c*(a+b-c)*x3) / s
py = (a*(b+c-a)*y1 + b*(c+a-b)*y2 + c*(a+b-c)*y3) / s
ar = a**0.5
br = b**0.5
cr = c**0.5
r = ar*br*cr / ((ar+br+cr)*(-ar+br+cr)*(ar-br+cr)*(ar+br-cr))**0.5
print("Radius of the said circle:")
print("{:>.3f}".format(r))
print("Central coordinate (x, y) of the circle:")
print("{:>.3f}".format(px), "{:>.3f}".format(py))

# 40. check whether a point (x,y) is in a triangle or not. There is a triangle formed by three points
print("Input x1,y1,x2,y2,x3,y3,xp,yp:")
x1, y1, x2, y2, x3, y3, xp, yp = map(float, input().split())
c1 = (x2-x1)*(yp-y1)-(y2-y1)*(xp-x1)
c2 = (x3-x2)*(yp-y2)-(y3-y2)*(xp-x2)
c3 = (x1-x3)*(yp-y3)-(y1-y3)*(xp-x3)
if (c1 < 0 and c2 < 0 and c3 < 0) or (c1 > 0 and c2 > 0 and c3 > 0):
    print("The point is in the triangle.")
else:
    print("The point is outside the triangle.")

# 41. compute and print sum of two given integers (more than or equal to zero). If given integers or the sum have more than 80 digits, print "overflow"
print("Input first integer:")
x = int(input())
print("Input second integer:")
y = int(input())
if x >= 10 ** 80 or y >= 10 ** 80 or x + y >= 10 ** 80:
    print("Overflow!")
else:
    print("Sum of the two integers: ", x + y)

# 42. program that accepts six numbers as input and sorts them in descending order
print("Input six integers:")
nums = sorted(map(int, input().split()))
nums.reverse()
print("After sorting the said ntegers:")
print(*nums)

# 43.  test whether two lines PQ and RS are parallel. The four points are P(x1, y1), Q(x2, y2), R(x3, y3), S(x4, y4).
print("Input x1,y1,x2,y2,x3,y3,xp,yp:")
x1, y1, x2, y2, x3, y3, x4, y4 = map(float, input().split())
print('PQ and RS are parallel.' if abs((x2 - x1)*(y4 - y3) -
                                       (x4 - x3)*(y2 - y1)) < 1e-10 else 'PQ and RS are not parallel')

# 44. find the maximum sum of a contiguous subsequence from a given sequence of numbers a1, a2, a3, ... an. A subsequence of one element is also a continuous subsequence. Go to the editor
# Input:
# You can assume that 1 <= n <= 5000 and -100000 <= ai <= 100000.
# Input numbers are separated by a space.
# Input 0 to exit.
# Input number of sequence of numbers you want to input (0 to exit):
# 3
# Input numbers:
# 2
# 4
# 6
# Maximum sum of the said contiguous subsequence: 12
# Input number of sequence of numbers you want to input (0 to exit):
# 0
numlist = [-2, -3, 4, -1, -2, 1, 5, -3]
print(numlist)
sumlst = []

for i, j in itertools.product(range(len(numlist)), range(2, len(numlist)+1)):
    x = sum(numlist[i:j])
    sumlst.append(x)

print("Maximum sum is : ", max(sumlst))

# 45. There are two circles C1 with radius r1, central coordinate (x1, y1) and C2 with radius r2 and central coordinate (x2, y2).
# Write a Python program to test the followings -

#     "C2 is in C1" if C2 is in C1
#     "C1 is in C2" if C1 is in C2
#     "Circumference of C1 and C2 intersect" if circumference of C1 and C2 intersect, and
#     "C1 and C2 do not overlap" if C1 and C2 do not overlap.
print("Input x1, y1, r1, x2, y2, r2:")
x1, y1, r1, x2, y2, r2 = [float(i) for i in input().split()]
d = math.sqrt((x1-x2)**2 + (y1-y2)**2)
if d < r1-r2:
    print("C2  is in C1")
elif d < r2-r1:
    print("C1  is in C2")
elif d > r1+r2:
    print("Circumference of C1  and C2  intersect")
else:
    print("C1 and C2  do not overlap")

print("Input month and date (separated by a single space):")
m, d = map(int, input().split())
weeks = {1: 'Monday', 2: 'Tuesday', 3: 'Wednesday',
         4: 'Thursday', 5: 'Friday', 6: 'Saturday', 7: 'Sunday'}
w = date.isoweekday(date(2016, m, d))
print("Name of the date: ", weeks[w])

# 47. program which reads a text (only alphabetical characters and spaces.) and prints two words. The first one is the word which is arise most frequently in the text. The second one is the word which has the maximum number of letters.
# Input:
# A text is given in a line with following condition:
# a. The number of letters in the text is less than or equal to 1000.
# b. The number of letters in a word is less than or equal to 32.
# c. There is only one word which is arise most frequently in given text.
# d. There is only one word which has the maximum number of letters in given text.
# Input text: Thank you for your comment and your participation.
# Output: your participation.
print("Input a text in a line.")
text_list = list(map(str, input().split()))
sc = collections.Counter(text_list)
common_word = sc.most_common()[0][0]
max_char = ""
for s in text_list:
    if len(max_char) < len(s):
        max_char = s
print("\nMost frequent text and the word which has the maximum number of letters.")
print(common_word, max_char)

# 48. program that reads n digits (given) chosen from 0 to 9 and prints the number of combinations where the sum of the digits equals to another given number (s). Do not use the same digits in a combination
# Two integers as number of combinations and their sum by a single space in a line. Input 0 0 to exit.
# Input number of combinations and sum, input 0 0 to exit:
# 5 6
# 2 4
# 0 0
# 2
print("Input number of combinations and sum, input 0 0 to exit:")
while True:
    x, y = map(int, input(). split())
    if x == 0 and y == 0:
        break
    s = list(itertools.combinations(range(10), x))
    ctr = sum(sum(i) == y for i in s)
print(ctr)

# 49.  program which reads the two adjoined sides and the diagonal of a parallelogram and check whether the parallelogram is a rectangle or a rhombus.
print("Input two adjoined sides and the diagonal of a parallelogram (comma separated):")
a, b, c = map(int, input().split(","))
# Using Pythagoras Thm,
if c**2 == a**2+b**2:
    print("This is a rectangle.")
# Since all four sides of rhombus are equal in length.
if a == b:
    print("This is a rhombus.")

# 50. program to replace a string "Python" with "Java" and "Java" with "Python" in a given string.
n = input('Input a text with two words "Python" and "Java"')
new = n.split()
for i in new:
    if i == 'Python':
        new[new.index(i)] = 'Java'
    elif i == 'Java':
        new[new.index(i)] = 'Python'
result = ' '.join(new)
print(result)

# 51.  find the difference between the largest integer and the smallest integer which are created by 8 numbers from 0 to 9. The number that can be rearranged shall start with 0 as in 00135668
print("Input an integer created by 8 numbers from 0 to 9.:")
num = list(input())
print("Difference between the largest and the smallest integer from the given integer:")
print(int("".join(sorted(num, reverse=True))) - int("".join(sorted(num))))

# 52. compute the sum of first n given prime numbers
MAX = 105000
print("Input a number (n≤10000) to compute the sum:(0 to exit)")
is_prime = [True for _ in range(MAX)]
is_prime[0] = is_prime[1] = False
for i in range(2, int(MAX ** (1 / 2)) + 1):
    if is_prime[i]:
        for j in range(i ** 2, MAX, i):
            is_prime[j] = False
primes = [i for i in range(MAX) if is_prime[i]]
while True:
    n = int(input())
    if not n:
        break
    print("Sum of first", n, "prime numbers:")
    print(sum(primes[:n]))

# 53. program that accept an even number (>=4, Goldbach number) from the user and create a combinations that express the given number as a sum of two prime numbers. Print the number of combinations
print("Input an even number (0 to exit):")
ub = 50000
is_prime = [0, 0, 1, 1] + [0]*(ub-3)
is_prime[5::6] = is_prime[7::6] = [1] * (ub // 6)
primes = [2, 3]
append = primes.append

for n in chain(range(5, ub, 6), range(7, ub, 6)):
    if is_prime[n]:
        append(n)
        is_prime[n*3::n*2] = [0]*((ub-n)//(n*2))
primes.sort()

for n in map(int, sys.stdin):
    if not n:
        break
    print("Number of combinations:")
    if n % 2:
        print(is_prime[n-2])
    else:
        print(len([1 for p in primes[:bisect_right(primes, n/2)] if is_prime[n-p]]))

while True:
    print("Input number of straight lines (o to exit): ")
    n = int(input())
    if n <= 0:
        break
    print("Number of regions:")
    print((n**2 + n + 2) // 2)

# 55. There are four different points on a plane, P(xp,yp), Q(xq, yq), R(xr, yr) and S(xs, ys). Write a Python program to test AB and CD are orthogonal or not.
while True:
    try:
        print("Input xp, yp, xq, yq, xr, yr, xs, ys:")
        x_p, y_p, x_q, y_q, x_r, y_r, x_s, y_s = map(float, input().split())
        pq_x, pq_y = x_q - x_p, y_q - y_p
        rs_x, rs_y = x_s - x_r, y_s - y_r
        rs = pq_x*rs_x + pq_y*rs_y
        if abs(rs) > 1e-10:
            print("AB and CD are not orthogonal!")
        else:
            print("AB and CD are orthogonal!")
    except:
        break

# 56. sum of all numerical values (positive integers) embedded in a sentence.
    # Write a Python program to create maximum number of regions obtained by drawing n given straight lines
    # CONTAINS REGEX IMPLEMENTATION!!!


def test(stri):
    print("Input some text and numeric values (<ctrl-d> to exit):")
    print("Sum of the numeric values: ", sum(
        [sum(map(int, re.findall(r"[0-9]{1,5}", stri)))]))


print(test("sd1fdsfs23 dssd56"))
print(test("15apple2banana"))
print(test("flowers5fruit5"))


# 57. There are 10 vertical and horizontal squares on a plane. Each square is painted blue and green. Blue represents the sea, and green represents the land. When two green squares are in contact with the top and bottom, or right and left, they are said to be ground. The area created by only one green square is called "island". For example, there are five islands in the figure below.
# Write a Python program to read the mass data and find the number of islands.
c = 0


def f(x, y, z):
    if 0 <= y < 10 and 0 <= z < 10 and x[z][y] == '1':
        x[z][y] = '0'
        for dy, dz in [[-1, 0], [1, 0], [0, -1], [0, 1]]:
            f(x, y+dy, z+dz)


print("Input 10 rows of 10 numbers representing green squares (island) as 1 and blue squares (sea) as zeros")
while 1:
    try:
        if c:
            input()
    except:
        break
    x = [list(input()) for _ in [0]*10]
    c = 1
    b = 0
    for i, j in itertools.product(range(10), range(10)):
        if x[j][i] == '1':
            b += 1
            f(x, i, j)
    print("Number of islands:")
    print(b)

# 58. When character are consecutive in a string , it is possible to shorten the character string by replacing the character with a certain rule. For example, in the case of the character string YYYYY, if it is expressed as # 5 Y, it is compressed by one character.
    # Write a Python program to restore the original string by entering the compressed string with this rule. However, the # character does not appear in the restored character string. Go to the editor
    # Note: The original sentences are uppercase letters, lowercase letters, numbers, symbols, less than 100 letters, and consecutive letters are not more than 9 letters.


def restore_original_str(a1):
    result = ""
    ind = 0
    end = len(a1)
    while ind < end:
        if a1[ind] == "#":
            result += a1[ind + 2] * int(a1[ind + 1])
            ind += 3
        else:
            result += a1[ind]
            ind += 1
    return result


print("Original text:", "XY#6Z1#4023")
print(restore_original_str("XY#6Z1#4023"))
print("Original text:", "#39+1=1#30")
print(restore_original_str("#39+1=1#30"))

# 59. A convex polygon is a simple polygon in which no line segment between two points on the boundary ever goes outside the polygon. Equivalently, it is a simple polygon whose interior is a convex set. In a convex polygon, all interior angles are less than or equal to 180 degrees, while in a strictly convex polygon all interior angles are strictly less than 180 degrees.
# Write a Python program that compute the area of the polygon . The vertices have the names vertex 1, vertex 2, vertex 3, ... vertex n according to the order of edge connections Go to the editor
# Note: The original sentences are uppercase letters, lowercase letters, numbers, symbols, less than 100 letters, and consecutive letters are not more than 9 letters.


def poly_area(c):
    add = []
    for i in range(0, (len(c) - 2), 2):
        add.extend(
            (
                c[i] * c[i + 3] - c[i + 1] * c[i + 2],
                c[len(c) - 2] * c[1] - c[len(c) - 1] * c[0],
            )
        )
        return abs(sum(add) / 2)


print(poly_area([1, 0, 0, 0, 1, 1, 2, 0, -1, 1]))

# 60. Internet search engine giant, such as Google accepts web pages around the world and classify them, creating a huge database. The search engines also analyze the search keywords entered by the user and create inquiries for database search. In both cases, complicated processing is carried out in order to realize efficient retrieval, but basics are all cutting out words from sentences.
# Write a Python program to cut out words of 3 to 6 characters length from a given sentence not more than 1024 characters

print("Input a sentence (1024 characters. max.)")
yy = input()
yy = yy.replace(",", " ")
yy = yy.replace(".", " ")
print("3 to 6 characters length of words:")
print(*[y for y in yy.split() if 3 <= len(y) <= 6])

#####EXTRA PROBLEM#####
'''LCM and GCD'''


def gcd(x, y):
    # Assign z the highest value, either x or y
    if x % y == 0:
        return y
    return next(
        (z for z in range(int(y / 2), 0, -1) if x % z == 0 and y % z == 0), 1
    )


def lcm(x, y):
    z = max(x, y)
    while (True):
        if (z % x == 0 and z % y == 0):
            lcm = z
            break
        z += 1

    return lcm


print(gcd(14, 42))
print(lcm(12, 17))

# 61. Arrange integers (0 to 99) as narrow hilltop, as illustrated in Figure 1. Reading such data representing huge, when starting from the top and proceeding according to the next rule to the bottom. Write a Python program that compute the maximum value of the sum of the passing integers
print("Input the numbers (ctrl+d to exit):")
nums = [list(map(int, l.split(","))) for l in sys.stdin]
mvv = nums[0]

for i in range(1, (len(nums)+1)//2):
    rvv = [0]*(i+1)
    for j in range(i):
        rvv[j] = max(rvv[j], mvv[j]+nums[i][j])
        rvv[j+1] = max(rvv[j+1], mvv[j]+nums[i][j+1])
    mvv = rvv

for i in range((len(nums)+1)//2, len(nums)):
    rvv = [0]*(len(mvv)-1)
    for j in range(len(rvv)):
        rvv[j] = max(mvv[j], mvv[j+1]) + nums[i][j]
    mvv = rvv
print("Maximum value of the sum of integers passing according to the rule on one line.")
print(mvv[0])


# 62. program to find the number of combinations that satisfy p + q + r + s = n where n is a given number <= 4000 and p, q, r, s in the range of 0 to 1000.
print("Input a positive integer: (ctrl+d to exit)")
pair_dict = Counter()
for i in range(2001):
    pair_dict[i] = min(i, 2000 - i) + 1

while True:
    try:
        n = int(input())
        ans = sum(pair_dict[i] * pair_dict[n - i] for i in range(n + 1))
        print("Number of combinations of a,b,c,d:", ans)
    except EOFError:
        break

# 63.  program which adds up columns and rows of given table as shown in the specified figure.
while True:
    print("Input number of rows/columns (0 to exit)")
    n = int(input())
    if n == 0:
        break
    print("Input cell value:")
    x = [[int(num) for num in input().split()] for _ in range(n)]
    for i in range(n):
        sum = 0
        for j in range(n):
            sum += x[i][j]
        x[i].append(sum)

    x.append([])
    for i in range(n + 1):
        sum = 0
        for j in range(n):
            sum += x[j][i]
        x[n].append(sum)
    print("Result:")
    for i in range(n + 1):
        for j in range(n + 1):
            print('{0:>5}'.format(x[i][j]), end="")
        print()


# 64.  Given a list of numbers and a number k, write a Python program to check whether the sum of any two numbers from the list is equal to k or not
def check_sum(nums, k):
    for i in range(len(nums)):
        for j in range(i+1, len(nums)):
            if nums[i] + nums[j] == k:
                return True
    return False


print(check_sum([12, 5, 0, 5], 10))
print(check_sum([20, 20, 4, 5], 40))
print(check_sum([1, -1], 0))
print(check_sum([1, 1, 0], 0))

# 65. In mathematics, a subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements. For example, the sequence (A,B,D) is a subsequence of (A,B,C,D,E,F) obtained after removal of elements C, E, and F. The relation of one sequence being the subsequence of another is a preorder.
# The subsequence should not be confused with substring (A,B,C,D) which can be derived from the above string (A,B,C,D,E,F) by deleting substring (E,F). The substring is a refinement of the subsequence.
# The list of all subsequences for the word "apple" would be "a", "ap", "al", "ae", "app", "apl", "ape", "ale", "appl", "appe", "aple", "apple", "p", "pp", "pl", "pe", "ppl", "ppe", "ple", "pple", "l", "le", "e", "".
# Write a Python program to find the longest word in set of words which is a subsequence of a given string.


def longest_word_sequence(s, d):
    long_word = ""

    for word in d:
        temp_word = ''
        j = 0
        for letter in word:

            for i in range(j, len(s)):

                if letter == s[i]:
                    temp_word += letter
                    j = i
                    break
                else:
                    continue

        if (temp_word) == word and len(long_word) < len(temp_word):
            long_word = temp_word

        else:
            continue
    return long_word


print(longest_word_sequence("Green", {"Gn", "Gren", "ree", "en"}))
print(longest_word_sequence("pythonexercises", {"py", "ex", "exercises"}))


# 66. A happy number is defined by the following process:
# Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers.
# Write a Python program to check whether a number is "happy" or not.
def is_Happy_num(n):
    past = set()
    while n != 1:
        n = sum(int(i)**2 for i in str(n))
        if n in past:
            return False
        past.add(n)
    return True


print(is_Happy_num(7))
print(is_Happy_num(932))
print(is_Happy_num(6))


# 67. program to find and print the first 10 happy numbers
def happy_numbers(n):
    past = set()
    while n != 1:
        n = sum(int(i)**2 for i in str(n))
        if n in past:
            return False
        past.add(n)
    return True


print([x for x in range(500) if happy_numbers(x)][:10])


# 68. program to count the number of prime numbers less than a given non-negative number
def count_Primes_nums(n):
    ctr = 0

    for num in range(n):
        if num <= 1:
            continue
        for i in range(2, num):
            if (num % i) == 0:
                break
        else:
            ctr += 1

    return ctr


print(count_Primes_nums(10))
print(count_Primes_nums(100))


# # 69. In abstract algebra, a group isomorphism is a function between two groups that sets up a one-to-one correspondence between the elements of the groups in a way that respects the given group operations. If there exists an isomorphism between two groups, then the groups are called isomorphic.
# Two strings are isomorphic if the characters in string A can be replaced to get string B
# Given "foo", "bar", return false.
# Given "paper", "title", return true.
# Write a Python program to check if two given strings are isomorphic to each other or not
# Write a Python program to find the longest common prefix string amongst a given array of strings. Return false If there is no common prefix.
# For Example, longest common prefix of "abcdefgh" and "abcefgh" is "abc"
def isIsomorphic(str1, str2):
    dict_str1 = {}
    dict_str2 = {}

    for i, value in enumerate(str1):
        dict_str1[value] = dict_str1.get(value, []) + [i]

    for j, value in enumerate(str2):
        dict_str2[value] = dict_str2.get(value, []) + [j]

    return sorted(dict_str1.values()) == sorted(dict_str2.values())


print(isIsomorphic("foo", "bar"))
print(isIsomorphic("bar", "foo"))
print(isIsomorphic("paper", "title"))
print(isIsomorphic("title", "paper"))
print(isIsomorphic("apple", "orange"))
print(isIsomorphic("aa", "ab"))
print(isIsomorphic("ab", "aa"))

# 70. program to find the longest common prefix string amongst a given array of strings. Return false If there is no common prefix.
# For Example, longest common prefix of "abcdefgh" and "abcefgh" is "abc"


def longest_Common_Prefix(str1):
    if not str1:
        return ""
    short_str = min(str1, key=len)
    for i, char in enumerate(short_str):
        for other in str1:
            if other[i] != char:
                return short_str[:i]
    return short_str


print(longest_Common_Prefix(["abcdefgh", "abcefgh"]))
print(longest_Common_Prefix(["w3r", "w3resource"]))
print(longest_Common_Prefix(["Python", "PHP", "Perl"]))
print(longest_Common_Prefix(["Python", "PHP", "Java"]))

# 71. program to reverse only the vowels of a given string


def reverse_vowels(str1):
    vowels = "".join(char for char in str1 if char in "aeiouAEIOU")
    result_string = ""
    for char in str1:
        if char in "aeiouAEIOU":
            result_string += vowels[-1]
            vowels = vowels[:-1]
        else:
            result_string += char
    return result_string


print(reverse_vowels("w3resource"))
print(reverse_vowels("Python"))
print(reverse_vowels("Perl"))
print(reverse_vowels("USA"))

# 72. program to check whether a given integer is a palindrome or not.


def is_Palindrome(n):
    return str(n) == str(n)[::-1]


print(is_Palindrome(100))
print(is_Palindrome(252))
print(is_Palindrome(-838))

# 73. program to remove the duplicate elements of a given array of numbers such that each element appear only once and return the new length of the given array.


def remove_duplicates(nums):
    for i in range(len(nums)-1, 0, -1):
        if nums[i] == nums[i-1]:
            del nums[i-1]
    return len(nums)


print(remove_duplicates([0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4]))
print(remove_duplicates([1, 2, 2, 3, 4, 4]))

# 74. program to calculate the maximum profit from selling and buying values of stock. An array of numbers represent the stock prices in chronological order. For example, given [8, 10, 7, 5, 7, 15], the function will return 10, since the buying value of the stock is 5 dollars and sell value is 15 dollars.


def buy_and_sell(stock_price):
    max_profit_val, current_max_val = 0, 0
    for price in reversed(stock_price):
        current_max_val = max(current_max_val, price)
        potential_profit = current_max_val - price
        max_profit_val = max(potential_profit, max_profit_val)
    return max_profit_val


print(buy_and_sell([8, 10, 7, 5, 7, 15]))
print(buy_and_sell([1, 2, 8, 1]))
print(buy_and_sell([]))

# 75. program to remove all instances of a given value from a given array of integers and find the length of the new array.


def remove_element(array_nums, val):
    i = 0
    while i < len(array_nums):
        if array_nums[i] == val:
            array_nums.remove(array_nums[i])
        else:
            i += 1
    return len(array_nums)


print(remove_element([1, 2, 3, 4, 5, 6, 7, 5], 5))
print(remove_element([10, 10, 10, 10, 10], 10))
print(remove_element([10, 10, 10, 10, 10], 20))
print(remove_element([], 1))

# 76.  program to find the starting and ending position of a given value in a given array of integers, sorted in ascending order


def search_Range(array_nums, target_val):
    start_pos = 0
    end_pos = 0
    for i in range(len(array_nums)):
        if target_val == array_nums[i]:
            if start_pos == -1:
                start_pos = i
            end_pos = i
    return [start_pos, end_pos]


print(search_Range([5, 7, 7, 8, 8, 8], 8))
print(search_Range([1, 3, 6, 9, 13, 14], 4))
print(search_Range([5, 7, 7, 8, 10], 8))

# 77.
'''SOLUTION ALREADY COVERED UNDER Q. 74'''


# 78. program to print a given N by M matrix of numbers line by line in forward > backwards > forward >... order.
def print_matrix(nums):
    flag = True

    for line in nums:

        if flag == True:
            for i in range(len(line)):
                print(line[i])
            flag = False

        else:
            i = -1
            while i > -1 * len(line) - 1:
                print(line[i])
                i -= 1
            flag = True


print_matrix([[1, 2, 3, 4],
              [5, 6, 7, 8],
              [0, 6, 2, 8],
              [2, 3, 0, 2]])

# 79. program to compute the largest product of three integers from a given list of integers.


def largest_product_of_three(nums):
    max_val = nums[1]
    for i in range(len(nums)):
        for j in range(i+1, len(nums)):
            for k in range(j+1, len(nums)):
                max_val = max(nums[i] * nums[j] * nums[k], max_val)
    return max_val


print(largest_product_of_three([-10, -20, 20, 1]))
print(largest_product_of_three([-1, -1, 4, 2, 1]))
print(largest_product_of_three([1, 2, 3, 4, 5, 6]))

# 80.  program to find the first missing positive integer that does not exist in a given list


def first_missing_number(nums):
    if len(nums) == 0:
        return 1
    nums.sort()
    smallest_int_num = 0
    for i in range(len(nums) - 1):
        if (
            nums[i] > 0
            and nums[i] != nums[i + 1]
            and nums[i + 1] - nums[i] != 1
        ):
            return nums[i] + 1
    if smallest_int_num == 0:
        smallest_int_num = nums[-1] + 1
    return smallest_int_num


print(first_missing_number([2, 3, 7, 6, 8, -1, -10, 15, 16]))
print(first_missing_number([1, 2, 4, -7, 6, 8, 1, -10, 15]))
print(first_missing_number([1, 2, 3, 4, 5, 6, 7]))
print(first_missing_number([-2, -3, -1, 1, 2, 3]))

# 81. randomly generate a list with 10 even numbers between 1 and 100 inclusive
print(random.sample([i for i in range(1, 100) if i % 2 == 0], 10))

# 82.
'''ALREADY COVERED PREVIOUSLY'''

# 83. test whether a given number is symmetrical or not.


def is_symmetrical_num(n):
    return str(n) == str(n)[::-1]


print(is_symmetrical_num(121))
print(is_symmetrical_num(0))
print(is_symmetrical_num(122))
print(is_symmetrical_num(990099))
# Basically, a palindorme program.

# 84. accept a list of numbers and create a list to store the count of negative number in first element and store the sum of positive numbers in second element.


def count_sum(nums):
    return (
        [len([n for n in nums if n < 0]), sum(n for n in nums if n > 0)]
        if nums
        else []
    )


print(count_sum([1, 2, 3, 4, 5]))
print(count_sum([-1, -2, -3, -4, -5]))
print(count_sum([1, 2, 3, -4, -5]))
print(count_sum([1, 2, -3, -4, -5]))

# 85. An isogram (also known as a "nonpattern word") is a logological term for a word or phrase without a repeating letter. It is also used by some people to mean a word or phrase in which each letter appears the same number of times, not necessarily just once. Conveniently, the word itself is an isogram in both senses of the word, making it autological.
# Write a Python program to check whether a given string is an "isogram" or not


def check_isogram(str1):
    return len(str1) == len(set(str1.lower()))


print(check_isogram("w3resource"))
print(check_isogram("w3r"))
print(check_isogram("Python"))
print(check_isogram("Java"))

# 86. count the number of equal numbers from three given integers


def test_three_equal(x, y, z):
    result = {x, y, z}
    return 0 if len(result) == 3 else (4 - len(result))


print(test_three_equal(1, 1, 1))
print(test_three_equal(1, 2, 2))
print(test_three_equal(-1, -2, -3))
print(test_three_equal(-1, -1, -1))

# 87. check whether a given employee code is exactly 8 digits or 12 digits. Return True if the employee code is valid and False if it's not.


def is_valid_emp_code(emp_code):
    return len(emp_code) in {8, 12} and emp_code.isdigit()


print(is_valid_emp_code('12345678'))
print(is_valid_emp_code('1234567j'))
print(is_valid_emp_code('12345678j'))
print(is_valid_emp_code('123456789123'))
print(is_valid_emp_code('123456abcdef'))

# 88. accept two strings and test if the letters in the second string are present in the first string


def string_letter_check(list_data):
    return all(char in list_data[0].lower() for char in list_data[1].lower())


print(string_letter_check(["python", "ypth"]))
print(string_letter_check(["python", "ypths"]))
print(string_letter_check(["python", "ypthon"]))
print(string_letter_check(["123456", "01234"]))
print(string_letter_check(["123456", "1234"]))

# 89. compute the sum of the three lowest positive numbers from a given list of numbers


def sum_three_smallest_nums(lst):
    return sum(sorted([x for x in lst if x > 0])[:3])


print(sum_three_smallest_nums([10, 20, 30, 40, 50, 60, 7]))
print(sum_three_smallest_nums([1, 2, 3, 4, 5]))
print(sum_three_smallest_nums([0, 1, 2, 3, 4, 5]))


# 90. replace all but the last five characters of a given string into "*" and returns the new masked string.
def mask_string(str1):
    return '*'*(len(str1)-5) + str1[-5:]


print(mask_string("kdi39323swe"))
print(mask_string("12345abcdef"))
print(mask_string("12345"))

# 91. count the number of arguments in a given function.


def num_of_args(*args):
    return(len(args))


print(num_of_args())
print(num_of_args(1))
print(num_of_args(1, 2))
print(num_of_args(1, 2, 3))
print(num_of_args(1, 2, 3, 4))
print(num_of_args([1, 2, 3, 4]))


# 92.  compute cumulative sum of numbers of a given list.
# Note: Cumulative sum = sum of itself + all previous numbers in the said list
def nums_cumulative_sum(nums_list):
    return [sum(nums_list[:i+1]) for i in range(len(nums_list))]


print(nums_cumulative_sum([10, 20, 30, 40, 50, 60, 7]))
print(nums_cumulative_sum([1, 2, 3, 4, 5]))
print(nums_cumulative_sum([0, 1, 2, 3, 4, 5]))

# 93.  find the middle character(s) of a given string. If the length of the string is odd return the middle character and return the middle two characters if the string length is even


def middle_char(txt):
    return txt[(len(txt)-1)//2:(len(txt)+2)//2]


print(middle_char("Python"))
print(middle_char("PHP"))
print(middle_char("Java"))

# 94. find the largest product of the pair of adjacent elements from a given list of integers.


def adjacent_num_product(list_nums):
    return max(a*b for a, b in zip(list_nums, list_nums[1:]))


print(adjacent_num_product([1, 2, 3, 4, 5, 6]))
print(adjacent_num_product([1, 2, 3, 4, 5]))
print(adjacent_num_product([2, 3]))

# 95. check whether every even index contains an even number and every odd index contains odd number of a given list.


def odd_even_position(nums):
    return all(nums[i] % 2 == i % 2 for i in range(len(nums)))


print(odd_even_position([2, 1, 4, 3, 6, 7, 6, 3]))
print(odd_even_position([2, 1, 4, 3, 6, 7, 6, 4]))
print(odd_even_position([4, 1, 2]))

# 96. check whether a given number is a narcissistic number or not.


def is_narcissistic_num(num):
    return num == sum(int(x) ** len(str(num)) for x in str(num))


print(is_narcissistic_num(153))
print(is_narcissistic_num(370))
print(is_narcissistic_num(407))
print(is_narcissistic_num(409))
print(is_narcissistic_num(1634))
print(is_narcissistic_num(8208))
print(is_narcissistic_num(9474))
print(is_narcissistic_num(9475))

# 97. find the highest and lowest number from a given string of space separated integers


def highest_lowest_num(str1):
    num_list = list(map(int, str1.split()))
    return max(num_list), min(num_list)


print(highest_lowest_num("1 4 5 77 9 0"))
print(highest_lowest_num("-1 -4 -5 -77 -9 0"))
print(highest_lowest_num("0 0"))


# 98. check whether a sequence of numbers has an increasing trend or not.
def increasing_trend(nums):
    return sorted(nums) == nums


print(increasing_trend([1, 2, 3, 4]))
print(increasing_trend([1, 2, 5, 3, 4]))
print(increasing_trend([-1, -2, -3, -4]))
print(increasing_trend([-4, -3, -2, -1]))
print(increasing_trend([1, 2, 3, 4, 0]))

# 99. program to find the position of the second occurrence of a given string in another given string. If there is no such string return -1


def find_string(txt, str1):
    return txt.find(str1, txt.find(str1)+1)


print(find_string("The quick brown fox jumps over the lazy dog", "the"))
print(find_string("the quick brown fox jumps over the lazy dog", "the"))

# 100. compute the sum of all items of a given array of integers where each integer is multiplied by its index. Return 0 if there is no number


def sum_index_multiplier(nums):
    return sum(j*i for i, j in enumerate(nums))


print(sum_index_multiplier([1, 2, 3, 4]))
print(sum_index_multiplier([-1, -2, -3, -4]))
print(sum_index_multiplier([]))

# 101. find the name of the oldest student from a given dictionary containing the names and ages of a group of students.


def oldest_student(students):
    return max(students, key=students.get)


print(oldest_student({"Bernita Ahner": 12, "Kristie Marsico": 11,
                      "Sara Pardee": 14, "Fallon Fabiano": 11,
                      "Nidia Dominique": 15}))
print(oldest_student({"Nilda Woodside": 12, "Jackelyn Pineda": 12.2,
                      "Sofia Park": 12.4, "Joannie Archibald": 12.6,
                      "Becki Saunder": 12.7}))

# 102. create a new string with no duplicate consecutive letters from a given string.


def no_consecutive_letters(txt):
    return txt[0] + ''.join(txt[i] for i in range(1, len(txt)) if txt[i] != txt[i-1])


print(no_consecutive_letters("PPYYYTTHON"))
print(no_consecutive_letters("PPyyythonnn"))
print(no_consecutive_letters("Java"))
print(no_consecutive_letters("PPPHHHPPP"))

# 103. check whether two given lines are parallel or not.


def parallel_lines(line1, line2):
    return line1[0]/line1[1] == line2[0]/line2[1]


# 2x + 3y = 4, 2x + 3y = 8
print(parallel_lines([2, 3, 4], [2, 3, 8]))
# 2x + 3y = 4, 4x - 3y = 8
print(parallel_lines([2, 3, 4], [4, -3, 8]))

# 104.  find the lucky number(s) in a given matrix.
# Lucky number in a Matrix: Maximum in its column and minimum in its row.


def lucky_Numbers(matrix):
    result = set(map(min, matrix)) & set(map(max, zip(*matrix)))
    return list(result)


m1 = [[1, 2], [2, 3]]
print("Original matrix:", m1)
print("Lucky number(s) in the said matrix: ", lucky_Numbers(m1))

m1 = [[1, 2, 3], [3, 4, 5]]
print("\nOriginal matrix:", m1)
print("Lucky number(s) in the said matrix: ", lucky_Numbers(m1))

m1 = [[7, 5, 6], [3, 4, 4], [6, 5, 7]]
print("\nOriginal matrix:", m1)
print("Lucky number(s) in the said matrix: ", lucky_Numbers(m1))

# 105. check whether a given sequence is linear, quadratic or cubic.


def Seq_Linear_Quadratic_Cubic(seq_nums):
    seq_nums = [seq_nums[x] - seq_nums[x-1] for x in range(1, len(seq_nums))]
    if len(set(seq_nums)) == 1:
        return "Linear Sequence"
    seq_nums = [seq_nums[x] - seq_nums[x-1] for x in range(1, len(seq_nums))]
    if len(set(seq_nums)) == 1:
        return "Quadratic Sequence"
    seq_nums = [seq_nums[x] - seq_nums[x-1] for x in range(1, len(seq_nums))]
    if len(set(seq_nums)) == 1:
        return "Cubic Sequence"


print(Seq_Linear_Quadratic_Cubic([0, 2, 4, 6, 8, 10]))
print(Seq_Linear_Quadratic_Cubic([1, 4, 9, 16, 25]))
print(Seq_Linear_Quadratic_Cubic([0, 12, 10, 0, -12, -20]))
print(Seq_Linear_Quadratic_Cubic([1, 2, 3, 4, 5]))

# 106. test whether a given integer is pandigital number or not.


def is_pandigital_num(n):
    return len(set(str(n))) == 10


print(is_pandigital_num(1023456897))
print(is_pandigital_num(1023456798))
print(is_pandigital_num(1023457689))
print(is_pandigital_num(1023456789))
print(is_pandigital_num(102345679))

# 107. check whether a given number is Oddish or Evenish.


def oddish_evenish_num(n):
    return 'Oddish' if sum(map(int, str(n))) % 2 else 'Evenish'


print(oddish_evenish_num(120))
print(oddish_evenish_num(321))
print(oddish_evenish_num(43))
print(oddish_evenish_num(4433))
print(oddish_evenish_num(373))

# 108. rogram that takes three integers and check whether the last digit of first number * the last digit of second number = the last digit of third number.


def check_last_digit(x, y, z):
    return str(x*y)[-1] == str(z)[-1]


print(check_last_digit(12, 22, 44))
print(check_last_digit(145, 122, 1010))
print(check_last_digit(0, 22, 40))
print(check_last_digit(1, 22, 40))
print(check_last_digit(145, 122, 101))


# 109. program find the indices of all occurrences of a given item in a given list.
def indices_in_list(nums_list, n):
    return [idx for idx, i in enumerate(nums_list) if i == n]


print(indices_in_list([1, 2, 3, 4, 5, 2], 2))
print(indices_in_list([3, 1, 2, 3, 4, 5, 6, 3, 3], 3))
print(indices_in_list([1, 2, 3, -4, 5, 2, -4], -4))

# 110. program to remove two duplicate numbers from a given number of list.


def two_unique_nums(nums):
    return [i for i in nums if nums.count(i) == 1]


print(two_unique_nums([1, 2, 3, 2, 3, 4, 5]))
print(two_unique_nums([1, 2, 3, 2, 4, 5]))
print(two_unique_nums([1, 2, 3, 4, 5]))

# 111. check whether two given circles (given center (x,y) and radius) are intersecting. Return true for intersecting otherwise false.


def is_circle_collision(circle1, circle2):
    x1, y1, r1 = circle1
    x2, y2, r2 = circle2
    distance = ((x1-x2)**2 + (y1-y2)**2)**0.5
    return distance <= r1 + r2


print(is_circle_collision([1, 2, 4], [1, 2, 8]))
print(is_circle_collision([0, 0, 2], [10, 10, 5]))

# 112. compute the digit distance between two integers.


def digit_distance_nums(n1, n2):
    return sum(map(int, str(abs(n1-n2))))


print(digit_distance_nums(123, 256))
print(digit_distance_nums(23, 56))
print(digit_distance_nums(1, 2))
print(digit_distance_nums(24232, 45645))

# 113. to reverse all the words which have even length.


def reverse_even(txt):
    return ' '.join(i if len(i) % 2 else i[::-1] for i in txt.split())


print(reverse_even("The quick brown fox jumps over the lazy dog"))
print(reverse_even("Python Exercises"))

# 114. to print letters from the English alphabet from a-z and A-Z
print("Alphabet from a-z:")
for letter in string.ascii_lowercase:
    print(letter, end=" ")
print("\nAlphabet from A-Z:")
for letter in string.ascii_uppercase:
    print(letter, end=" ")

# 115. generate and prints a list of numbers from 1 to 10.
nums = range(1, 10)
print(list(nums))
print(list(map(str, nums)))

# 116. identify nonprime numbers between 1 to 100 (integers). Print the nonprime numbers.


def is_not_prime(n):
    return any(n % i == 0 for i in range(2, int(math.sqrt(n)) + 1))


print("Nonprime numbers between 1 to 100:")
for x in filter(is_not_prime, range(1, 101)):
    print(x)

# 117. make a request to a web page, and test the status code, also display the html code of the specified web page.
url = 'http://www.example.com/'
headers = {'User-Agent': 'Mozilla/5.0 (Macintosh) Gecko/20100101 Firefox/38.0'}
request = requests.get(url, headers=headers)
print("Web page status: ", request)
print("\nHTML code of the above web page:")
if request.ok:
    print(request.text)

# 118. In multiprocessing, processes are spawned by creating a Process object. Write a Python program to show the individual process IDs (parent process, process id etc.) involved


def info(title):
    print(title)
    print('module name:', __name__)
    print('parent process:', os.getppid())
    print('process id:', os.getpid())


def f(name):
    info('function f')
    print('hello', name)


if __name__ == '__main__':
    info('Main line')
    p = Process(target=f, args=('bob',))
    p.start()
    p.join()

# 119. to check if two given numbers are coprime or not. Return True if two numbers are coprime otherwise return false.


def gcd(p, q):
    # Create the gcd of two positive integers.
    while q != 0:
        p, q = q, p % q
    return p


def is_coprime(x, y):
    return gcd(x, y) == 1


print(is_coprime(17, 13))
print(is_coprime(17, 21))
print(is_coprime(15, 21))
print(is_coprime(25, 45))

# 120. calculate Euclid's totient function of a given integer. Use a primitive method to calculate Euclid's totient function.


def gcd(p, q):
    # Create the gcd of two positive integers.
    while q != 0:
        p, q = q, p % q
    return p


def is_coprime(x, y):
    return gcd(x, y) == 1


def phi_func(x):
    if x == 1:
        return 1
    n = [y for y in range(1, x) if is_coprime(x, y)]
    return len(n)


print(phi_func(10))
print(phi_func(15))
print(phi_func(33))




BeautifulSoup4.py # 1. Write a Python program to find the title tags from a given html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("Title of the document:")
print(soup.find("title"))


# 2. Write a Python program to retrieve all the paragraph tags from a given html document. 
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("All paragraph tags:")
print(soup.find_all("p"))

# 3. Write a Python program to get the number of paragraph tags of a given html document. 
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("Number of paragraph tags:")
print(len(soup.find_all("p")))

# 4. Write a Python program to extract the text in the first paragraph tag of a given html document. 
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("The text in the first paragraph tag:")
print(soup.find_all('p')[0].text)

# 5. Write a Python program to find the length of the text of the first < h2 > tag of a given html document. 
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("Length of the text of the first <h2> tag:")
print(len(soup.find('h2').text))

# 6. Write a Python program to find the text of the first < a > tag of a given html text. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("Text of the first <a> tag:")
print(soup.find('a').text)

# 7. Write a Python program to find the href of the first < a > tag of a given html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print("href of the first <a> tag:")
print(soup.find('a').attrs['href'])

# 8. Write a Python program to extract all the URLs from the webpage python.org that are nested within < li > tags from . 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')

urls = []
for h in soup.find_all('li'):
    a = h.find('a')
    urls.append(a.attrs['href'])
print(urls)

# 9. Write a Python program to find all the h2 tags and list the first four from the webpage python.org. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("First four h2 tags from the webpage python.org.:")
print(soup.find_all('h2')[:4])

# 10. Write a Python program to find all the link tags and list the first ten from the webpage python.org. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("First four h2 tags from the webpage python.org.:")
print(soup.find_all('a')[:10])

# 11. Write a Python program to a list of all the h1, h2, h3 tags from the webpage python.org. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("List of all the h1, h2, h3 :")
for heading in soup.find_all(["h1", "h2", "h3"]):
    print(f'{heading.name} {heading.text.strip()}')

# 12. Write a Python program to extract all the text from a given web page. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("Text from the said page:")
print(soup.get_text())

# 13. Write a Python program to print the names of all HTML tags of a given web page going through the document tree. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nNames of all HTML tags (https://www.python.org):\n")
for child in soup.recursiveChildGenerator():
    if child.name:
        print(child.name)

# 14. Write a Python program to retrieve children of the html tag from a given web page. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nChildren of the html tag (https://www.python.org):\n")
root = soup.html
root_childs = [e.name for e in root.children if e.name is not None]
print(root_childs)

# 15. Write a Python program to retrieve all descendants of the body tag from a given web page. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nDescendants of the body tag (https://www.python.org):\n")
root = soup.html
root_childs = [e.name for e in root.descendants if e.name is not None]
print(root_childs)

# 16. Write a Python program to retrieve the HTML code of the title, its text, and the HTML code of its parent. 
import requests
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("title")
print(soup.title)
print("title text")
print(soup.title.text)
print("Parent content of the title:")
print(soup.title.parent)

# 17. Write a Python program to find and print all li tags of a given web page. 
url = 'https://www.w3resource.com/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nFind and print all li tags:\n")
for tag in soup.find_all("li"):
    print("{0}: {1}".format(tag.name, tag.text))

# 18. Write a Python program to print content of elements that contain a specified string of a given web page. 
import requests
import re
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nContent of elements that contain 'Python' string:")
str1 = soup.find_all(string=re.compile('Python'))
for txt in str1:
    print(" ".join(txt.split()))

# 19. Write a Python program to print the element(s) that has a specified id of a given web page. 
import requests
import re
from bs4 import BeautifulSoup
url = 'https://www.python.org/'
reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'lxml')
print("\nelement(s) that has #python-network id:\n")
print(soup.select_one("#python-network"))

# 20. Write a Python program to create a Beautiful Soup parse tree into a nicely formatted Unicode string, with a separate line for each HTML/XML tag and string. 
from bs4 import BeautifulSoup
str1 = "<p>Some<b>bad<i>HTML Code</i></b></p>"
print("Original string:")
print(str1)
soup = BeautifulSoup("<p>Some<b>bad<i>HTML Code</i></b></p>", "xml")
print("\nFormatted Unicode string:")
print(soup.prettify())

# 21. Write a Python program to find the first tag with a given attribute value in an html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, "lxml")
print(soup.find( href="https://www.w3resource.com/css/CSS-tutorials.php"))

# 22. Write a Python program to find tag(s) beneath other tag(s) in a given html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc,"lxml")
print("\na tag(s) Beneath body tag:")
print(soup.select("body a"))
print("\nBeneath html head:")
print(soup.select("html head title"))

# 23. Write a Python program to find tag(s) directly beneath other tag(s) in a given html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
</body>
</html>
"""
soup = BeautifulSoup(html_doc,"lxml")
print("\nBeneath directly head tag:")
print(soup.select("head > title"))
print()
print("\nBeneath directly p tag:")
print(soup.select("p > a")) 

# 24. Write a Python program to find the siblings of tags in a given html document. 
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
<a class="sister" href="http://example.com/lacie" id="link1">Lacie</a>
<a class="sister" href="http://example.com/tillie"  id="link2">Tillie</a>
</body>
</html>
"""
soup = BeautifulSoup(html_doc, "lxml")
print("\nSiblings of tags:")
print(soup.select("#link1 ~ .sister"))
print(soup.select("#link1 + .sister"))

# 25. Write a Python program to find tags by CSS class in a given html document. 
from bs4 import BeautifulSoup
html_doc = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;
charset=iso-8859-1">
<title>An example of HTML page</title>
</head>
<body>
<h2>This is an example HTML page</h2>
<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc at nisi velit,
aliquet iaculis est. Curabitur porttitor nisi vel lacus euismod egestas. In hac
habitasse platea dictumst. In sagittis magna eu odio interdum mollis. Phasellus
sagittis pulvinar facilisis. Donec vel odio volutpat tortor volutpat commodo.
Donec vehicula vulputate sem, vel iaculis urna molestie eget. Sed pellentesque
adipiscing tortor, at condimentum elit elementum sed. Mauris dignissim
elementum nunc, non elementum felis condimentum eu. In in turpis quis erat
imperdiet vulputate. Pellentesque mauris turpis, dignissim sed iaculis eu,
euismod eget ipsum. Vivamus mollis adipiscing viverra. Morbi at sem eget nisl
euismod porta.</p>
<p><a href="https://www.w3resource.com/html/HTML-tutorials.php">Learn HTML from
w3resource.com</a></p>
<p><a class="sister" href="https://www.w3resource.com/css/CSS-tutorials.php">Learn CSS from 
w3resource.com</a></p>
<a class="sister" href="http://example.com/lacie" id="link1">Lacie</a>
<a class="sister" href="http://example.com/tillie"  id="link2">Tillie</a>
</body>
</html>
"""
soup = BeautifulSoup(html_doc,"lxml")
print("\nTags by CSS class:")
print(soup.select(".sister"))

# 26. Write a Python program to change the tag's contents and replace with the given string. 
from bs4 import BeautifulSoup
html_doc = '<a href="http://example.com/">HTML<i>example.com</i></a>'
soup = BeautifulSoup(html_doc, "lxml")
tag = soup.a
print("\nOriginal Markup:")
print(tag)
print("\nOriginal Markup with new text:")
tag.string = "CSS"
print(tag)

# 27. Write a Python program to add to a tag's contents in a given html document. 
from bs4 import BeautifulSoup
html_doc = '<a href="http://example.com/">HTML<i>w3resource.com</i></a>'
soup = BeautifulSoup(html_doc, "lxml")
print("\nOriginal Markup:")
print(soup.a)
soup.a.append("CSS")
print("\nAfter append a text in the new link:")
print(soup.a)

# 28. Write a Python program to insert a new text within a url in a specified position. 
from bs4 import BeautifulSoup
html_doc = '<a href="http://example.com/">HTML<i>w3resource.com</i></a>'
soup = BeautifulSoup(html_doc, "lxml")
tag = soup.a
print("Original Markup:")
print(tag.contents)
tag.insert(2, "CSS") #2-> Position of the text (1, 2, 3…)
print("\nNew url after inserting the text:")
print(tag.contents)

# 29. Write a Python program to insert tags or strings immediately before specified tags or strings. 
from bs4 import BeautifulSoup
soup = BeautifulSoup("<b>w3resource.com</b>", "lxml")
print("Original Markup:")
print(soup.b)
tag = soup.new_tag("i")
tag.string = "Python"
print("\nNew Markup, before inserting the text:")
soup.b.string.insert_before(tag)
print(soup.b)

# 30. Write a Python program to insert tags or strings immediately after specified tags or strings. 
from bs4 import BeautifulSoup
soup = BeautifulSoup("<b>w3resource.com</b>", "lxml")
print("Original Markup:")
print(soup.b)
tag = soup.new_tag("i")
tag.string = "Python"
print("\nNew Markup, after inserting the text:")
soup.b.string.insert_after(tag)
print(soup.b)

# 31. Write a Python program to remove the contents of a tag in a given html document. 
from bs4 import BeautifulSoup
html_content = '<a href="https://w3resource.com/">Python exercises<i>w3resource</i></a>'
soup = BeautifulSoup(html_content, "lxml")
print("Original Markup:")
print(soup.a)
tag = soup.a
tag = tag.clear()
print("\nAfter clearing the contents in the tag:")
print(soup.a)

# 32. Write a Python program to extract a tag or string from a given tree of html document. 
from bs4 import BeautifulSoup
html_content = '<a href="https://w3resource.com/">Python exercises<i>w3resource</i></a>'
soup = BeautifulSoup(html_content, "lxml")
print("Original Markup:")
print(soup.a)
i_tag = soup.i.extract()
print("\nExtract i tag from said html Markup:")
print(i_tag)

# 33. Write a Python program to remove a tag from a given tree of html document and destroy it and its contents. 
from bs4 import BeautifulSoup
html_content = '<a href="https://w3resource.com/">Python exercises<i>w3resource</i></a>'
soup = BeautifulSoup(html_content, "lxml")
print("Original Markup:")
a_tag = soup.a
print(a_tag)
new_tag = soup.a.decompose()
print("After decomposing:")
print(new_tag)

# 34. Write a Python program to remove a tag or string from a given tree of html document and replace it with the given tag or string. 
from bs4 import BeautifulSoup
html_markup= '<a href="https://w3resource.com/">Python exercises<i>w3resource</i></a>'
soup = BeautifulSoup(html_markup, "lxml")
print("Original markup:")
a_tag = soup.a
print(a_tag)
new_tag = soup.new_tag("b")
new_tag.string = "PHP"
b_tag = a_tag.i.replace_with(new_tag)
print("New Markup:")
print(a_tag)

# 35. Write a Python program to wrap an element in the specified tag and create the new wrapper. 
from bs4 import BeautifulSoup
soup = BeautifulSoup("<p>Python exercises.</p>", "lxml")
print("Original Markup:")
print(soup.p.string.wrap(soup.new_tag("i")))
print("\nNew Markup:")
print(soup.p.wrap(soup.new_tag("div")))

# 36. Write a Python program to replace a given tag with whatever's inside a given tag. 
from bs4 import BeautifulSoup
markup = '<a href="https://w3resource.com/">Python exercises.<i>w3resource.com</i></a>'
soup = BeautifulSoup(markup, "lxml")
a_tag = soup.a
print("Original markup:")
print(a_tag)
a_tag.i.unwrap()
print("\nAfter unwrapping:")
print(a_tag)





BinarySearchTree.py # 1. Write a Python program to create a Balanced Binary Search Tree (BST) using an array (given) elements where array elements are sorted in ascending order. 
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def BST_Sort(nums):
    
    if not nums:
        return None
    mid_val = len(nums)//2
    node = TreeNode(nums[mid_val])
    node.left = BST_Sort(nums[:mid_val])
    node.right = BST_Sort(nums[mid_val+1:])
    return node

def preOrder(node): 
    if not node: 
        return      
    print(node.val)
    preOrder(node.left) 
    preOrder(node.right)   
    
result = BST_Sort([1, 2, 3, 4, 5, 6, 7])
preOrder(result)

# 2. Write a Python program to find the closest value of a given target value in a given non-empty Binary Search Tree (BST) of unique values. 
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def closest_value(root, target):
    a = root.val
    kid = root.left if target < a else root.right
    if not kid:
        return a
    b = closest_value(kid, target)
    return min((a,b), key=lambda x: abs(target-x))

root = TreeNode(8)  
root.left = TreeNode(5)  
root.right = TreeNode(14) 
root.left.left = TreeNode(4)  
root.left.right = TreeNode(6) 
root.left.right.left = TreeNode(8)  
root.left.right.right = TreeNode(7)  
root.right.right = TreeNode(24) 
root.right.right.left = TreeNode(22)  
    
result = closest_value(root, 19)
print(result)

# 3. Write a Python program to check whether a given a binary tree is a valid binary search tree (BST) or not. 
# Let a binary search tree (BST) is defined as follows:
# The left subtree of a node contains only nodes with keys less than the node's key.
# The right subtree of a node contains only nodes with keys greater than the node's key.
# Both the left and right subtrees must also be binary search trees.

# Example 1:
#     2
#    / \
#   1   3
# Binary tree [2,1,3], return true.
# Example 2:
#     1
#    / \
#   2   3
# Binary tree [1,2,3], return false.
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def is_BST(root):
    stack = []
    prev = None
    
    while root or stack:
        while root:
            stack.append(root)
            root = root.left
        root = stack.pop()
        if prev and root.val <= prev.val:
            return False
        prev = root
        root = root.right
    return True

root = TreeNode(2)  
root.left = TreeNode(1)  
root.right = TreeNode(3) 
 
result = is_BST(root)
print(result)

root = TreeNode(1)  
root.left = TreeNode(2)  
root.right = TreeNode(3) 
 
result = is_BST(root)
print(result)

# 4. Write a Python program to delete a node with the given key in a given Binary search tree (BST). 
# Note: Search for a node to remove. If the node is found, delete the node.
# Definition: Binary tree node.
class TreeNode(object):
    def __init__(self, x):
         self.val = x
         self.left = None
         self.right = None

def delete_Node(root, key):
  # if root doesn't exist, just return it
	if not root: 
		return root
	# Find the node in the left subtree	if key value is less than root value
	if root.val > key: 
		root.left = delete_Node(root.left, key)
	# Find the node in right subtree if key value is greater than root value, 
	elif root.val < key: 
		root.right= delete_Node(root.right, key)
	# Delete the node if root.value == key
	else: 
	# If there is no right children delete the node and new root would be root.left
		if not root.right:
			return root.left
	# If there is no left children delete the node and new root would be root.right	
		if not root.left:
			return root.right
  # If both left and right children exist in the node replace its value with 
  # the minmimum value in the right subtree. Now delete that minimum node
  # in the right subtree
		temp_val = root.right
		mini_val = temp_val.val
		while temp_val.left:
			temp_val = temp_val.left
			mini_val = temp_val.val
  # Delete the minimum node in right subtree
		root.right = deleteNode(root.right,root.val)
	return root

def preOrder(node): 
    if not node: 
        return      
    print(node.val)
    preOrder(node.left) 
    preOrder(node.right)   
    
root = TreeNode(5)  
root.left = TreeNode(3)  
root.right = TreeNode(6) 
root.left.left = TreeNode(2)  
root.left.right = TreeNode(4) 
root.left.right.left = TreeNode(7)  
print("Original node:")
print(preOrder(root))
result = delete_Node(root, 4)
print("After deleting specified node:")
print(preOrder(result))


# 5. Write a Python program to convert a given array elements to a height balanced Binary Search Tree (BST). 
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def array_to_bst(array_nums):
    if not array_nums:
        return None
    mid_num = len(array_nums)//2
    node = TreeNode(array_nums[mid_num])
    node.left = array_to_bst(array_nums[:mid_num])
    node.right = array_to_bst(array_nums[mid_num+1:])
    return node

def preOrder(node): 
    if not node: 
        return      
    print(node.val)
    preOrder(node.left) 
    preOrder(node.right)   

array_nums = [1,2,3,4,5,6,7]

print("Original array:")
print(array_nums)
result = array_to_bst(array_nums)
print("\nArray to a height balanced BST:")
print(preOrder(result))

# 6. Write a Python program to find the kth smallest element in a given a binary search tree. 
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def kth_smallest(root, k):
    stack = []
    while root or stack:
        while root:
            stack.append(root)
            root = root.left
        root = stack.pop()
        k -= 1
        if k == 0:
            break
        root = root.right
    return root.val

root = TreeNode(8)  
root.left = TreeNode(5)  
root.right = TreeNode(14) 
root.left.left = TreeNode(4)  
root.left.right = TreeNode(6) 
root.left.right.left = TreeNode(8)  
root.left.right.right = TreeNode(7)  
root.right.right = TreeNode(24) 
root.right.right.left = TreeNode(22)  

print(kth_smallest(root, 2))
print(kth_smallest(root, 3))




Classes.py 

"""
1. Write a Python class to convert an integer to a roman numeral.
"""
		
class RomanNumeral:
    """Roman Numerals class
        Rules:
            I =	1
            V =	5
            X =	10
            L =	50
            C =	100
            D =	500
            M =	1000
        
        1. Repeating a numeral up to three times represents addition of the number.
            For example, III represents 1 + 1 + 1 = 3.
            Only I, X, C, and M can be repeated; V, L, and D cannot be, and there
            is no need to do so.
        
        2. Writing numerals that decrease from left to right represents addition of the numbers.
            For example, LX represents 50 + 10 = 60 and XVI represents 10 + 5 + 1 = 16.
        
        3. Writing a smaller numeral to the left of a larger numeral represents subtraction.
            For example, IV represents 5 - 1 = 4 and IX represents 10 - 1 = 9.
            To avoid ambiguity, the only pairs of numerals that use this subtraction rule are:
            IV	4   = 5 - 1
            IX	9   = 10 - 1
            XL	40  = 50 - 10
            XC	90  = 100 - 10
            CD	400 = 500 - 100
            CM	900 = 1000 - 100
            
        4. To represent larger numbers, a bar over a numeral means to multiply the number by 1000.
            For example, D(bar) represents 1000 x 500 = 500,000 and M(bar) represents 1000 x 1000
            = 1,000,000, one million.***
            
        *** this class only works for integers lower than 4000 (till 3999 including)
    """
    
    def __init__(self, integer):
        self.integer = integer
        self.roman = ''

        roman_numerals = ['I','IV','V','IX','X','XL','L','XC','C','CD', 'D', 'CM', 'M']
        integers =       [1,   4,   5,  9,  10,  40,  50, 90, 100, 400, 500, 900, 1000]

        n = self.integer
        for i,r in sorted(zip(integers, roman_numerals), reverse=True):
            while n//i > 0:
                self.roman += r
                n = n-i

x = int(input("please enter a number from 1 to 3999: "))         
var = RomanNumeral(x)
print("the Roman numeral of your number is:", var.roman)
print("your number in Hindu-Arabic numerals is:", var.integer)
##print(var.__doc__)

"""
2. Write a Python class to convert a roman numeral to an integer.
"""
class HinduArabic:
    """HinduArabic class lets you convert roman numeral to hindu-arabic numeral
        Rules:
            I =	1
            V =	5
            X =	10
            L =	50
            C =	100
            D =	500
            M =	1000
        
        1. Repeating a numeral up to three times represents addition of the number.
            For example, III represents 1 + 1 + 1 = 3.
            Only I, X, C, and M can be repeated; V, L, and D cannot be, and there
            is no need to do so.
        
        2. Writing numerals that decrease from left to right represents addition of the numbers.
            For example, LX represents 50 + 10 = 60 and XVI represents 10 + 5 + 1 = 16.
        
        3. Writing a smaller numeral to the left of a larger numeral represents subtraction.
            For example, IV represents 5 - 1 = 4 and IX represents 10 - 1 = 9.
            To avoid ambiguity, the only pairs of numerals that use this subtraction rule are:
            IV	4   = 5 - 1
            IX	9   = 10 - 1
            XL	40  = 50 - 10
            XC	90  = 100 - 10
            CD	400 = 500 - 100
            CM	900 = 1000 - 100
            
        4. To represent larger numbers, a bar over a numeral means to multiply the number by 1000.
            For example, D(bar) represents 1000 x 500 = 500,000 and M(bar) represents 1000 x 1000
            = 1,000,000, one million.***
            
        *** this class only works for integers lower than 4000 (till 3999 including)
    """
    
    def __init__(self, roman):
        self.integer = int()
        self.roman = roman

        roman_numerals = ['IV','IX','XL','XC','CD','CM','I','V','X','L','C','D','M']
        
        integers       = [  4,   9,  40,  90,  400, 900, 1,  5, 10, 50, 100, 500, 1000]

        n = self.roman
        for i,r in zip(integers, roman_numerals):
            while n.rfind(r) != -1:
                self.integer += i
                n = n.replace(r , '', 1)

x = input("please enter a roman numeral using I, V, X, L, C, D, M letters only: ")        
var = HinduArabic(x)
print("your number in Hindu-Arabic numerals is:", var.integer)
print("the Roman numeral of your number is:", var.roman)
##print(var.__doc__)

"""
3. Write a Python class to find validity of a string of parentheses, '(', ')', '{', '}', '[' and '].
These brackets must be close in the correct order, for example "()" and "()[]{}" and "([]){}"are valid but
"[)", "({[)]" and "{{{" are invalid.
"""

class Parenthesis:
    def __init__(self, par_string):
        self.par_string = par_string
        
# not the most elegant solution, but it works. there is a great solution on w3resource site at 
# https://www.w3resource.com/python-exercises/class-exercises/python-class-exercise-3.php
    def is_valid(self):
        p = self.par_string
        open_par  = ['(', '[', '{']
        close_par = [')', ']', '}']
        if len(p) % 2 != 0 or p[0] in close_par or p[-1] in open_par:
            # check if length is not even, the string is invalid, no need to check further
            return False
        wrong_par = ['(]', '(}', '[)', '[}', '{)', '{]']

        # check if there is a wrong pair of open+close parenthesis from a wrong couples list
        for par in wrong_par:
            if p.find(par) != -1:
                return False
            # check if all closing parentheses have their opening counterpart before them
        for o, c in zip(open_par, close_par):
            while c in p:
                i = p.find(c)
                print(i)
                j = p.rfind(o, 0, i)
                print(j)
                if j == -1:
                    return False
                p = p.replace(o, '', 1)
                p = p.replace(c, '', 1)
                print(p)
        return p == ''
                            
print(Parenthesis("{}{{}}{{").is_valid())

"""
4. Write a Python class to get all possible unique subsets from a set of distinct integers.
Input : [4, 5, 6]
Output : [[], [6], [5], [5, 6], [4], [4, 6], [4, 5], [4, 5, 6]]
"""

class Subsets:

    def all_subsets(self, my_set):
        output = [[]]
        for i in range(len(my_set)):
            output.append([my_set[i]])
            k = len(my_set)
            while k>=i:
                output.extend([my_set[i]]+my_set[j:k] for j in range(i+1, k))
                k -= 1
        return sorted(output)


print(Subsets().all_subsets([4, 5, 6]))

        
"""
5. Write a Python class to find a pair of elements (indices of the two numbers) from a
given array whose sum equals a specific target number. 
Input: numbers= [10,20,10,40,50,60,70], target=50
Output: 3, 4
"""

# solution 1 (finds first pair only)
class EqualsTarget:
    def find_pair(self, iterable, target):
        for i in iterable:
            for j in iterable[iterable.index(i)+1:]:
                if i+j == target:
                    return iterable.index(i), iterable.index(j)
        return -1, -1

print(EqualsTarget().find_pair([10,20,10,40,50,60,70], 50))

# solution 2 (finds all pairs)
class EqualsTarget:
    def find_pair(self, iterable, target):
        pairs = []
        for i,n in enumerate(iterable):
            pairs.extend((i, j+i+1) for j, m in enumerate(iterable[i+1:]) if n+m == target)
        return pairs

print(EqualsTarget().find_pair([-10,10,20,10,40,50,60,70,0,-20], 50))

"""
6. Write a Python class to find the three elements that sum to zero from a set
of n real numbers. 
Input array : [-25, -10, -7, -3, 2, 4, 8, 10]
Output : [[-10, 2, 8], [-7, -3, 10]]
"""

class SumZero:
    def three_sum_zero(self, iterable):
        triples = []
        for i, n in enumerate(iterable):
            for j, m in enumerate(iterable[i+1:]):
                triples.extend([n, m, y] for y in iterable[i+1+j+1:] if n+m+y == 0)
        return triples


print(SumZero().three_sum_zero([-25, -10, -7, -3, 2, 4, 8, 10]))


"""
7. Write a Python class to implement pow(x, n).
"""

class ImplementPow:
    def imp_pow(self, x, n):
        return x ** n

print(ImplementPow().imp_pow(3, 4))

"""
8. Write a Python class to reverse a string word by word. - Go to the editor
Input string : 'hello .py'
Expected Output : '.py hello'
"""

# solution 1
class ReverseString:
    def reversed(self, astring):
        split_string = astring.split()
        return ''.join(f'{word} ' for word in split_string[::-1])

print(ReverseString().reversed('hello .py nice to meet you'))

# solution 2
class ReverseString:
    def reversed(self, astring):
        split_string = astring.split()
        return ' '.join(reversed(split_string))

print(ReverseString().reversed('hello .py nice to meet you'))
        
"""
9. Write a Python class which has two methods get_String and print_String.
get_String accepts a string from the user and print_String prints the string in upper case.
"""

class MyString:
    def __init__(self):
        self.string = ''
        
    def get_String(self):
        s = input('input your string: ')
        self.string = s

    def print_String(self):
        print(self.string.upper())

var = MyString()
var.get_String()
var.print_String()
print(var.string)

"""
10. Write a Python class named Rectangle constructed by a length and width and
a method which will compute the area of a rectangle.
"""

class Rectangle:
    def __init__(self, l, w):
        self.length = l
        self.width = w
        
    def area(self):
        return self.length * self.width

X = Rectangle(4,5)
print(X.area())

"""
11. Write a Python class named Circle constructed by a radius and two methods which
will compute the area and the perimeter of a circle.
"""
from math import pi

class Circle:
    def __init__(self, r):
        self.radius = r

    def area(self):
        return round(pi * self.radius * self.radius, 2)
    
    def perimeter(self):
        return round(2 * pi * self.radius, 2)

X = Circle(8)
print(X.area())
print(X.perimeter())
print(X.radius)

"""
12. Write a Python program to get the class name of an instance in Python.
"""
"""
instance.__class__
The class to which a class instance belongs.
definition.__name__
The name of the class, function, method, descriptor, or generator instance.
"""
class Circle:
    """ construct a circle """
    def __init__(self, r):
        self.radius = r
    def area(self):
        return round(3.1415 * self.radius * self.radius, 2)
    
    def perimeter(self):
        return round(2 * 3.1415 * self.radius, 2)

X = Circle(8)
print(X.__class__)
print(type(X).__name__)
##print(type(X).__bases__)
##print(type(X).__dict__)

'''EXTRA: EXCERCISE ABOUT PRIVATE AND PUBLIC MEMBERS'''
class Vehicle:
    def __init__(self, color, maxSpeed):
        self.color=color
        self.__maxSpeed=int(maxSpeed)
    '''
    We use getters and setters to access private class members in subclass defined in parent class.
    Getters --> to return the value while printing
    Setters --> to update the value of the private member of class
    Pay careful attention to syntax
    '''
    
    '''
    NOTE: Single Underscore for Protected members (for programmmer's convenience and double underscore for private members, known by compiler.) 
    _ is protected
    __ is private  
    '''

    def getMaxSpeed(self):
        return self.__maxSpeed

    def setMaxSpeed(self, maxSpeed):
         self.__maxSpeed=maxSpeed

class Car(Vehicle):    
    def __init__(self, color, maxSpeed,  numGears, isConvertible):
        #inheriting the members from the Vehicle class using super().__init__(x,y)
        super().__init__(color, maxSpeed)
        self.numGears=int(numGears)
        self.isConvertible=isConvertible
    
    def printCar(self):
        print("Color: ", self.color)
        print("Max Speed: ", self.getMaxSpeed(), " kmph")
        print("Number of Gears: ", self.numGears)
        print("Convertible?: ", self.isConvertible)

c=Car('Purple', 390, 6, 'Y')
c.printCar()
        



Collections.py # Collections module implements specialized container datatypes providing alternatives to Python's general purpose built-in containers, dict, list, set, and tuple.

# 1. Write a Python program that iterate over elements repeating each as many times as its count. 
# Sample Output: ['p', 'p', 'p', 'p', 'q', 'q']
from itertools import chain
import collections as ct
import collections
from collections import Counter, OrderedDict
import re
from collections import Counter
c = Counter(p=4, q=2, r=0, s=-2)
print(list(c.elements()))



# 2. Write a Python program to find the most common elements and their counts of a specified text. 
# Original string: lkseropewdssafsdfafkpwe
# Most common three characters of the said string:
# [('s', 4), ('e', 3), ('f', 3)]
s = 'lkseropewdssafsdfafkpwe'
print(f"Original string: {s}")
print("Most common three characters of the said string:")
print(Counter(s).most_common(3))


# 3. Write a Python program to create a new deque with three items and iterate over the deque's elements. 
# Sample Output:
# a
# e
# i
# o
# u
from collections import deque
dq = deque('aeiou')
for element in dq:
   print(element)


# 4. Write a Python program to find the occurrences of 10 most common words in a given text. 
# Sample Output:
# [('Python', 6), ('the', 6), ('and', 5), ('We', 2), ('with', 2),
#  ('The', 1), ('Software', 1), ('Foundation', 1), ('PSF', 1), ('is', 1)]
text = """The Python Software Foundation (PSF) is a 501(c)(3) non-profit 
corporation that holds the intellectual property rights behind
the Python programming language. We manage the open source licensing 
for Python version 2.1 and later and own and protect the trademarks 
associated with Python. We also run the North American PyCon conference 
annually, support other Python conferences around the world, and 
fund Python related development with our grants program and by funding 
special projects."""
words = re.findall('\w+', text)
print(Counter(words).most_common(10))


class OrderedCounter(Counter, OrderedDict):
   pass


n = int(input("Input number of words: "))
print("Input the words: ")
word_array = [input().strip() for _ in range(n)]
word_ctr = OrderedCounter(word_array)
print(len(word_ctr))
for word in word_ctr:
   print(word_ctr[word], end=' ')


# 6. Write a Python program that accept name of given subject and marks. Input number of subjects in first line and subject name, marks separated by a space in next line. Print subject name and marks in order of its first occurrence. 
# Sample Output:
# Powered by
# Number of subjects: 3
# Input Subject name and marks: Bengali 58
# Input Subject name and marks: English 62
# Input Subject name and marks: Math 68
# Bengali 58
# English 62
# Math 68
n = int(input("Number of subjects: "))
item_order = collections.OrderedDict()
for _ in range(n):
   sub_marks_list = re.split(
       r'(\d+)$', input("Input Subject name and marks: ").strip())
   subject_name = sub_marks_list[0]
   item_price = int(sub_marks_list[1])
   item_order[subject_name] = (item_price if subject_name not in item_order
                               else item_order[subject_name] + item_price)
for i in item_order:
   print(i+str(item_order[i]))


# 7. Write a Python program to create a deque and append few elements to the left and right, then remove some elements from the left, right sides and reverse the deque. 
# Sample Output:
# deque(['Red', 'Green', 'White'])
# Adding to the left:
# deque(['Pink', 'Red', 'Green', 'White'])
# Adding to the right:
# deque(['Pink', 'Red', 'Green', 'White', 'Orange'])
# Removing from the right:
# deque(['Pink', 'Red', 'Green', 'White'])
# Removing from the left:
# deque(['Red', 'Green', 'White'])
# Reversing the deque:
# deque(['White', 'Green', 'Red'])
import collections
# Create a deque
deque_colors = collections.deque(["Red","Green","White"])
print(deque_colors)
# Append to the left
print("\nAdding to the left: ")
deque_colors.appendleft("Pink")
print(deque_colors)
# Append to the right
print("\nAdding to the right: ")
deque_colors.append("Orange")
print(deque_colors)
# Remove from the right
print("\nRemoving from the right: ")
deque_colors.pop()
print(deque_colors)
# Remove from the left
print("\nRemoving from the left: ")
deque_colors.popleft()
print(deque_colors)
# Reverse the dequeue
print("\nReversing the deque: ")
deque_colors.reverse()
print(deque_colors)


# 8. Write a Python program to create a deque from an existing iterable object. 
# Sample Output:
# Original tuple:
# (2, 4, 6)
# <class 'tuple' >
# Original deque:
# deque([2, 4, 6])
# New deque from an existing iterable object:
# deque([2, 2, 4, 6, 8, 10, 12])
# <class 'collections.deque' >
import collections
even_nums = (2, 4, 6)
print("Original tuple:")
print(even_nums)
print(type(even_nums))
even_nums_deque = collections.deque(even_nums)
print("\nOriginal deque:")
print(even_nums_deque)
even_nums_deque.append(8)
even_nums_deque.append(10)
even_nums_deque.append(12)
even_nums_deque.appendleft(2)
print("New deque from an existing iterable object:")
print(even_nums_deque)
print(type(even_nums_deque))


# 9. Write a Python program to add more number of elements to a deque object from an iterable object. 
# Sample Output:
# Even numbers:
# deque([2, 4, 6, 8, 10])
# More even numbers:
# deque([2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
even_nums = (2, 4, 6, 8, 10)
even_deque = collections.deque(even_nums)
print("Even numbers:")
print(even_deque)
more_even_nums = (12, 14, 16, 18, 20)
even_deque.extend(more_even_nums)
print("More even numbers:")
print(even_deque)


# 10. Write a Python program to remove all the elements of a given deque object. 
# Sample Output:
# Original Deque object with odd numbers:
# deque([1, 3, 5, 7, 9])
# Deque length: 5
# Deque object after removing all numbers - deque([])
# Deque length: 0
import collections
odd_nums = (1,3,5,7,9)
odd_deque  = collections.deque(odd_nums)
print("Original Deque object with odd numbers:")
print(odd_deque)
print("Deque length: %d"%(len(odd_deque)))
odd_deque.clear()
print("Deque object after removing all numbers-")
print(odd_deque)
print("Deque length:%d"%(len(odd_deque)))


# 11. Write a Python program to copy of a deque object and verify the shallow copying process. 
# Sample Output:
# Content of dq1:
# deque([1, 3, 5, 7, 9])
# dq2 id:
# 140706429557152
# Content of dq2:
# deque([1, 3, 5, 7, 9])
# dq2 id:
# 140706406914152
# Checking the first element of dq1 and dq2 are shallow copies:
# 11065888
# 11065888
import collections
tup1 = (1,3,5,7,9)
dq1 = collections.deque(tup1)
dq2 = dq1.copy()
print("Content of dq1:")
print(dq1)
print("dq2 id:")
print(id(dq1))
print("\nContent of dq2:")
print(dq2)
print("dq2 id:")
print(id(dq2))
print("\nChecking the first element of dq1 and dq2 are shallow copies:")
print(id(dq1[0]))
print(id(dq2[0]))


# 12. Write a Python program to count the number of times a specific element presents in a deque object. 
# Sample Output:
# Number of 2 in the sequence
# 5
# Number of 4 in the sequence
# 4
import collections
nums = (2,9,0,8,2,4,0,9,2,4,8,2,0,4,2,3,4,0)
nums_dq = collections.deque(nums)
print("Number of 2 in the sequence")
print(nums_dq.count(2))
print("Number of 4 in the sequence")
print(nums_dq.count(4))


# 13. Write a Python program to rotate a Deque Object specified number(positive) of times. 
# Sample Output:
# Deque before rotation:
# deque([2, 4, 6, 8, 10])
# Deque after 1 positive rotation:
# deque([10, 2, 4, 6, 8])
# Deque after 2 positive rotations:
# deque([6, 8, 10, 2, 4])
import collections
# declare an empty deque object
dq_object = collections.deque()
# Add elements to the deque - left to right
dq_object.append(2)
dq_object.append(4)
dq_object.append(6)
dq_object.append(8)
dq_object.append(10)
print("Deque before rotation:")
print(dq_object)
# Rotate once in positive direction
dq_object.rotate()
print("\nDeque after 1 positive rotation:")
print(dq_object)
# Rotate twice in positive direction
dq_object.rotate(2)
print("\nDeque after 2 positive rotations:")
print(dq_object)


# 14. Write a Python program to rotate a Deque Object specified number(negative) of times. 
# Sample Output:
# Deque before rotation:
# deque([2, 4, 6, 8, 10])
# Deque after 1 negative rotation:
# deque([4, 6, 8, 10, 2])
# Deque after 2 negative rotations:
# deque([8, 10, 2, 4, 6])
# declare an empty deque object
dq_object = collections.deque()
# Add elements to the deque - left to right
dq_object.append(2)
dq_object.append(4)
dq_object.append(6)
dq_object.append(8)
dq_object.append(10)
print("Deque before rotation:")
print(dq_object)
# Rotate once in negative direction
dq_object.rotate(-1)
print("\nDeque after 1 negative rotation:")
print(dq_object)
# Rotate twice in negative direction
dq_object.rotate(-2)
print("\nDeque after 2 negative rotations:")
print(dq_object)


# 15. Write a Python program to find the most common element of a given list. 
# Sample Output:
# Original list:
# ['PHP', 'PHP', 'Python', 'PHP', 'Python', 'JS', 'Python', 'Python', 'PHP', 'Python']
# Most common element of the said list:
# Python
language = ['PHP', 'PHP', 'Python', 'PHP', 'Python',
            'JS', 'Python', 'Python', 'PHP', 'Python']
print("Original list:")
print(language)
cnt = Counter(language)
print("\nMost common element of the said list:")
print(cnt.most_common(1)[0][0])


# 16. Write a Python program to perform Counter arithmetic and set operations for aggregating results. 
# Sample Output:
# C1: Counter({1: 1, 2: 1, 3: 1, 4: 1, 5: 1})
# C2: Counter({4: 1, 5: 1, 6: 1, 7: 1, 8: 1})
# Combined counts:
# Counter({4: 2, 5: 2, 1: 1, 2: 1, 3: 1, 6: 1, 7: 1, 8: 1})
# Subtraction:
# Counter({1: 1, 2: 1, 3: 1})
# Intersection(taking positive minimums):
# Counter({4: 1, 5: 1})
# Union(taking maximums):
# Counter({1: 1, 2: 1, 3: 1, 4: 1, 5: 1, 6: 1, 7: 1, 8: 1})
import collections
c1 = collections.Counter([1, 2, 3, 4, 5])
c2 = collections.Counter([4, 5, 6, 7, 8])
print('C1:', c1)
print('C2:', c2)
print('\nCombined counts:')
print(c1 + c2)
print('\nSubtraction:')
print(c1 - c2)
print('\nIntersection (taking positive minimums):')
print(c1 & c2)
print('\nUnion (taking maximums):')
print(c1 | c2)


# 17. Write a Python program to find the majority element from a given array of size n using Collections module. 
# Sample Output:
# 10
import collections
class Solution(object):
    def majorityElement(self, nums):
        """
        :type nums: List[int]
        :return type: int
        """
        count_ele=collections.Counter(nums)
        return count_ele.most_common()[0][0]

result = Solution().majorityElement([10,10,20,30,40,10,20,10])
print(result)


# 18. Write a Python program to merge more than one dictionary in a single expression. 
# Sample Output:
# Original dictionaries:
# {'R': 'Red', 'B': 'Black', 'P': 'Pink'} {'G': 'Green', 'W': 'White'}
# Merged dictionary:
# {'B': 'Black', 'R': 'Red', 'P': 'Pink', 'G': 'Green', 'W': 'White'}
# Original dictionaries:
# {'R': 'Red', 'B': 'Black', 'P': 'Pink'} {'G': 'Green', 'W': 'White'} {'O': 'Orange', 'W': 'White', 'B': 'Black'}
# Merged dictionary:
# {'B': 'Black', 'R': 'Red', 'P': 'Pink', 'G': 'Green', 'W': 'White', 'O': 'Orange'}
def merge_dictionaries(color1, color2):
   return dict(ct.ChainMap({}, color1, color2))


color1 = {"R": "Red", "B": "Black", "P": "Pink"}
color2 = {"G": "Green", "W": "White"}
print("Original dictionaries:")
print(color1, ' ', color2)
print("\nMerged dictionary:")
print(merge_dictionaries(color1, color2))


def merge_dictionaries(color1, color2, color3):
   return dict(ct.ChainMap({}, color1, color2, color3))


color1 = {"R": "Red", "B": "Black", "P": "Pink"}
color2 = {"G": "Green", "W": "White"}
color3 = {"O": "Orange", "W": "White", "B": "Black"}

print("\nOriginal dictionaries:")
print(color1, ' ', color2, color3)
print("\nMerged dictionary:")
# Duplicate colours have automatically removed.
print(merge_dictionaries(color1, color2, color3))


# 19. Write a Python program to break a given list of integers into sets of a given positive number. Return true or false. 
# Sample Output:
# Original list: [1, 2, 3, 4, 5, 6, 7, 8]
# Number to devide the said list: 4
# True
# Original list: [1, 2, 3, 4, 5, 6, 7, 8]
# Number to devide the said list: 3
# False
import collections as clt
def check_break_list(nums, n):
    coll_data = clt.Counter(nums)
    for x in sorted(coll_data.keys()):
        for index in range(1, n):
            coll_data[x+index] = coll_data[x+index]  - coll_data[x]
            if coll_data[x+index] < 0:
                return False
    return True

nums = [1,2,3,4,5,6,7,8]
n = 4
print("Original list:",nums)
print("Number to devide the said list:",n)
print(check_break_list(nums, n))
nums = [1,2,3,4,5,6,7,8]
n = 3
print("\nOriginal list:",nums)
print("Number to devide the said list:",n)
print(check_break_list(nums, n))


# 20. Write a Python program to find the item with maximum frequency in a given list. 
# Sample Output:
# Original list:
# [2, 3, 8, 4, 7, 9, 8, 2, 6, 5, 1, 6, 1, 2, 3, 2, 4, 6, 9, 1, 2]
# Item with maximum frequency of the said list:
# (2, 5)
from collections import defaultdict
def max_occurrences(nums):
   dict = defaultdict(int)
   for i in nums:
       dict[i] += 1
   return max(dict.items(), key=lambda x: x[1])
nums = [2,3,8,4,7,9,8,2,6,5,1,6,1,2,3,2,4,6,9,1,2]
print ("Original list:")
print(nums)
print("\nItem with maximum frequency of the said list:")
print(max_occurrences(nums))


# 21. Write a Python program to count most and least common characters in a given string. 
# Sample Output:
# Original string:
# hello world
# Most common character of the said string: l
# Least common character of the said string: h


def max_least_char(str1):
    temp = Counter(str1)
    max_char = max(temp, key=temp.get)
    min_char = min(temp, key=temp.get)
    return (max_char, min_char)


str1 = "hello world"
print("Original string: ")
print(str1)
result = max_least_char(str1)
print("\nMost common character of the said string:", result[0])
print("Least common character of the said string:", result[1])


# 22. Write a Python program to insert an element at the beginning of a given OrderedDictionary. 
# Sample Output:
# Original OrderedDict:
# OrderedDict([('color1', 'Red'), ('color2', 'Green'), ('color3', 'Blue')])
# Insert an element at the beginning of the said OrderedDict:
# Updated OrderedDict:
# OrderedDict([('color4', 'Orange'), ('color1', 'Red'),
#              ('color2', 'Green'), ('color3', 'Blue')])
from collections import OrderedDict
color_orderdict = OrderedDict([('color1', 'Red'), ('color2', 'Green'), ('color3', 'Blue')]) 
print("Original OrderedDict:")
print(color_orderdict)
print("Insert an element at the beginning of the said OrderedDict:")
color_orderdict.update({'color4':'Orange'})
color_orderdict.move_to_end('color4', last = False)
print("\nUpdated OrderedDict:")
print(color_orderdict)


# 23. Write a Python program to get the frequency of the tuples in a given list. 
# Sample Output:
# Original list of tuples:
# [(['1', '4'], ['4', '1'], ['3', '4'], ['2', '7'], [
#   '6', '8'], ['5', '8'], ['6', '8'], ['5', '7'], ['2', '7'])]
# Tuples frequency
# ('1', '4') 2
# ('3', '4') 1
# ('2', '7') 2
# ('6', '8') 2
# ('5', '8') 1
# ('5', '7') 1
from collections import Counter
nums = [(['1', '4'], ['4', '1'], ['3', '4'], ['2', '7'], ['6', '8'], ['5','8'], ['6','8'], ['5','7'], ['2','7'])]
print("Original list of tuples:")
print(nums)
result = Counter(tuple(sorted(i)) for i in nums[0])
print("\nTuples","    ","frequency")
for key,val in result.items():
    print(key," ", val)


# 24. Write a Python program to calculate the maximum aggregate from the list of tuples(pairs). 
# Sample Output:
# Original list:
# [('Juan Whelan', 90), ('Sabah Colley', 88), ('Peter Nichols', 7),
#  ('Juan Whelan', 122), ('Sabah Colley', 84)]
# Maximum aggregate value of the said list of tuple pair:
# ('Juan Whelan', 212)
from collections import defaultdict
def max_aggregate(st_data):
    temp = defaultdict(int)
    for name, marks in st_data:
        temp[name] += marks
    return max(temp.items(), key=lambda x: x[1])


students = [('Juan Whelan',90),('Sabah Colley',88),('Peter Nichols',7),('Juan Whelan',122),('Sabah Colley',84)]
print("Original list:")
print(students)
print("\nMaximum aggregate value of the said list of tuple pair:")
print(max_aggregate(students))


# 25. Write a Python program to find the characters in a list of strings which occur more than and less than a given number. 
# Sample Output:
# Original list:
# ['abcd', 'iabhef', 'dsalsdf', 'sdfsas', 'jlkdfgd']
# Characters of the said list of strings which occur more than: 3
# ['a', 'd', 'f']
# Characters of the said list of strings which occur less than: 3
# ['c', 'b', 'h', 'e', 'i', 's', 'l', 'k', 'j', 'g']


def max_aggregate(list_str, N):
    temp = (set(sub) for sub in list_str)
    counts = Counter(chain.from_iterable(temp))
    gt_N = [chr for chr, count in counts.items() if count > N]
    lt_N = [chr for chr, count in counts.items() if count < N]
    return gt_N, lt_N


list_str = ['abcd', 'iabhef', 'dsalsdf', 'sdfsas', 'jlkdfgd']
print("Original list:")
print(list_str)
N = 3
result = max_aggregate(list_str, N)
print("\nCharacters of the said list of strings which occur more than:", N)
print(result[0])
print("\nCharacters of the said list of strings which occur less than:", N)
print(result[1])


# 26. Write a Python program to find the difference between two list including duplicate elements. Use collections module. 
# Sample Output:
# Original lists:
# [3, 3, 4, 7]
from collections import Counter
l1 = [1,1,2,3,3,4,4,5,6,7]
l2 = [1,1,2,4,5,6]
print("Original lists:")
c1 = Counter(l1)
c2 = Counter(l2)
diff = c1-c2
print(list(diff.elements()))


# 27. Write a Python program to remove duplicate words from a given string use collections module. 
# Sample Output:
# Original String:
# Python Exercises Practice Solution Exercises
# After removing duplicate words from the said string:
# Python Exercises Practice Solution
from collections import OrderedDict
text_str = "Python Exercises Practice Solution Exercises"
print("Original String:")
print(text_str)
print("\nAfter removing duplicate words from the said string:")
result = ' '.join(OrderedDict((w,w) for w in text_str.split()).keys())
print(result)


# 28. Write a Python program to create a dictionary grouping a sequence of key-value pairs into a dictionary of lists. Use collections module. 
# Sample Output:
# Original list:
# [('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]
# Grouping a sequence of key-value pairs into a dictionary of lists:
# defaultdict( < class 'list' > , {'yellow': [1, 3], 'blue': [2, 4], 'red': [1]})
from collections import defaultdict
def grouping_dictionary(l):
    d = defaultdict(list)
    for k, v in l:
        d[k].append(v)
    return d
colors = [('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]
print("Original list:")
print(colors)
print("\nGrouping a sequence of key-value pairs into a dictionary of lists:")
print(grouping_dictionary(colors))


# 29. Write a Python program to get the frequency of the elements in a given list of lists. Use collections module. 
# Sample Output:
# Original list of lists:
# [[1, 2, 3, 2], [4, 5, 6, 2], [7, 1, 9, 5]]
# Frequency of the elements in the said list of lists:
# Counter({2: 3, 1: 2, 5: 2, 3: 1, 4: 1, 6: 1, 7: 1, 9: 1})
from collections import Counter
from itertools import chain
nums = [
        [1,2,3,2],
        [4,5,6,2],
        [7,1,9,5],
       ]
    
print("Original list of lists:")
print(nums)
print("\nFrequency of the elements in the said list of lists:")
result = Counter(chain.from_iterable(nums))
print(result)


# 30. Write a Python program to count the occurrence of each element of a given list. 
# Sample Output:
# Original List:
# ['Green', 'Red', 'Blue', 'Red', 'Orange', 'Black', 'Black', 'White', 'Orange']
# Count the occurrence of each element of the said list:
# Counter({'Red': 2, 'Orange': 2, 'Black': 2, 'Green': 1, 'Blue': 1, 'White': 1})
# Original List:
# [3, 5, 0, 3, 9, 5, 8, 0, 3, 8, 5, 8, 3, 5, 8, 1, 0, 2]
# Count the occurrence of each element of the said list:
# Counter({3: 4, 5: 4, 8: 4, 0: 3, 9: 1, 1: 1, 2: 1})
colors = ['Green', 'Red', 'Blue', 'Red',
          'Orange', 'Black', 'Black', 'White', 'Orange']
print("Original List:")
print(colors)
print("Count the occurrence of each element of the said list:")
result = Counter(colors)
print(result)
nums = [3, 5, 0, 3, 9, 5, 8, 0, 3, 8, 5, 8, 3, 5, 8, 1, 0, 2]
print("\nOriginal List:")
print(nums)
print("Count the occurrence of each element of the said list:")
result = Counter(nums)
print(result)


# 31. Write a Python program to count the most common words in a dictionary. 
# Sample Output:
# [('pink', 6), ('black', 5), ('white', 5), ('red', 4)]
words = [
   'red', 'green', 'black', 'pink', 'black', 'white', 'black', 'eyes',
   'white', 'black', 'orange', 'pink', 'pink', 'red', 'red', 'white', 'orange',
   'white', "black", 'pink', 'green', 'green', 'pink', 'green', 'pink',
   'white', 'orange', "orange", 'red'
]
from collections import Counter
word_counts = Counter(words)
top_four = word_counts.most_common(4)
print(top_four)


# 32. Write a Python program to find the class wise roll number from a tuple-of-tuples. 
# Sample Output:
# defaultdict( < class 'list' > , {'V': [1, 2], 'VI': [1, 2, 3], 'VII': [1]})
from collections import defaultdict
classes = (
    ('V', 1),
    ('VI', 1),
    ('V', 2),
    ('VI', 2),
    ('VI', 3),
    ('VII', 1),
)

class_rollno = defaultdict(list)

for class_name, roll_id in classes:
    class_rollno[class_name].append(roll_id)

print(class_rollno)


# 33. Write a Python program to count the number of students of individual class. 
# Sample Output:
# Counter({'VI': 3, 'V': 2, 'VII': 1})
from collections import Counter
classes = (
    ('V', 1),
    ('VI', 1),
    ('V', 2),
    ('VI', 2),
    ('VI', 3),
    ('VII', 1),
)
students = Counter(class_name for class_name, no_students in classes)
print(students)


# 34. Write a Python program to create an instance of an OrderedDict using a given dictionary. Sort the dictionary during the creation and print the members of the dictionary in reverse order. 
# Sample Output:
# Afghanistan 93
# Albania 355
# Algeria 213
# Andorra 376
# Angola 244
# In reverse order:
# Angola 244
# Andorra 376
# Algeria 213
# Albania 355
# Afghanistan 93
from collections import OrderedDict
dict = {'Afghanistan': 93, 'Albania': 355, 'Algeria': 213, 'Andorra': 376, 'Angola': 244}
new_dict = OrderedDict(dict.items())
for key in new_dict:
    print (key, new_dict[key])

print("\nIn reverse order:")
for key in reversed(new_dict):
    print (key, new_dict[key])


# 35. Write a Python program to group a sequence of key-value pairs into a dictionary of lists. 
# Sample Output:
# [('v', [1, 3]), ('vi', [2, 4]), ('vii', [1])]
from collections import defaultdict
class_roll = [('v', 1), ('vi', 2), ('v', 3), ('vi', 4), ('vii', 1)]
d = defaultdict(list)
for k, v in class_roll:
    d[k].append(v)
print(sorted(d.items()))


# 36. Write a Python program to compare two unordered lists(not sets). 
# Sample Output:
# False
def compare_lists(x, y):
    return Counter(x) == Counter(y)
n1 = [20, 10, 30, 10, 20, 30]
n2 = [30, 20, 10, 30, 20, 50]
print(compare_lists(n1, n2))




ConditionalStatementsAndLoops.py # 1. Write a Python program to find those numbers which are divisible by 7 and multiple of 5, between 1500 and 2700 (both included). 
import random
nl = []
for x in range(1500, 2701):
    if (x % 7 == 0) and (x % 5 == 0):
        nl.append(str(x))
print(','.join(nl))

# 2. Write a Python program to convert temperatures to and from celsius, fahrenheit. 
# [ Formula : c/5 = f-32/9 [ where c = temperature in celsius and f = temperature in fahrenheit ]
# Expected Output :
# 60°C is 140 in Fahrenheit
# 45°F is 7 in Celsius
temp = input(
    "Input the  temperature you like to convert? (e.g., 45F, 102C etc.) : ")
degree = int(temp[:-1])
i_convention = temp[-1]

if i_convention.upper() == "C":
  result = int(round((9 * degree) / 5 + 32))
  o_convention = "Fahrenheit"
elif i_convention.upper() == "F":
  result = int(round((degree - 32) * 5 / 9))
  o_convention = "Celsius"
else:
  print("Input proper convention.")
  quit()
print("The temperature in", o_convention, "is", result, "degrees.")


# 3. Write a Python program to guess a number between 1 to 9. 
# Note : User is prompted to enter a guess. If the user guesses wrong then the prompt appears again until the guess is correct, on successful guess, user will get a "Well guessed!" message, and the program will exit.
target_num, guess_num = random.randint(1, 10), 0
while target_num != guess_num:
    guess_num = int(
        input('Guess a number between 1 and 10 until you get it right : '))
print('Well guessed!')

# 4. Write a Python program to construct the following pattern, using a nested for loop.
n=5;
for i in range(n):
    for j in range(i):
        print ('* ', end="")
    print('')

for i in range(n,0,-1):
    for j in range(i):
        print('* ', end="")
    print('')
# * 
# * * 
# * * * 
# * * * * 
# * * * * * 
# * * * * 
# * * * 
# * * 
# *

# 5. Write a Python program that accepts a word from the user and reverse it. 
word = input("Input a word to reverse: ")
for char in range(len(word) - 1, -1, -1):
  print(word[char], end="")
print("\n")

# 6. Write a Python program to count the number of even and odd numbers from a series of numbers. 
# Sample numbers : numbers = (1, 2, 3, 4, 5, 6, 7, 8, 9) 
# Expected Output :
# Number of even numbers : 5
# Number of odd numbers : 4
numbers = (1, 2, 3, 4, 5, 6, 7, 8, 9)  # Declaring the tuple
count_odd = 0
count_even = 0
for x in numbers:
       if not x % 2:
    	     count_even += 1
       else:
    	     count_odd += 1
print("Number of even numbers :", count_even)
print("Number of odd numbers :", count_odd)

# 7. Write a Python program that prints each item and its corresponding type from the following list.
# Sample List : datalist = [1452, 11.23, 1+2j, True, 'w3resource', (0, -1), [5, 12], {"class":'V', "section":'A'}]
datalist = [1452, 11.23, 1+2j, True, 'w3resource', (0, -1), [5, 12],
            {"class": 'V', "section": 'A'}]
for item in datalist:
   print("Type of ", item, " is ", type(item))

# 8. Write a Python program that prints all the numbers from 0 to 6 except 3 and 6.
# Note : Use 'continue' statement.
# Expected Output : 0 1 2 4 5
for x in range(6):
    if (x == 3 or x == 6):
        continue
    print(x, end=' ')
print("\n")

# 9. Write a Python program to get the Fibonacci series between 0 to 50. 
# Note : The Fibonacci Sequence is the series of numbers :
# 0, 1, 1, 2, 3, 5, 8, 13, 21, ....
# Every next number is found by adding up the two numbers before it.
# Expected Output : 1 1 2 3 5 8 13 21 34
x, y = 0, 1
while y < 50:
    print(y)
    x, y = y, x+y

# 10. Write a Python program which iterates the integers from 1 to 50. For multiples of three print "Fizz" instead of the number and for the multiples of five print "Buzz". For numbers which are multiples of both three and five print "FizzBuzz".
# Sample Output :
# fizzbuzz
# 1
# 2
# fizz
# 4
# buzz
for fizzbuzz in range(51):
    if fizzbuzz %15 == 0:
        print("fizzbuzz")
        continue
    elif fizzbuzz % 3 == 0:
        print("fizz")
        continue
    elif fizzbuzz % 5 == 0:
        print("buzz")
        continue
    print(fizzbuzz)

# 11. Write a Python program which takes two digits m (row) and n (column) as input and generates a two-dimensional array. The element value in the i-th row and j-th column of the array should be i*j. 
# Note :
# i = 0,1.., m-1
# j = 0,1, n-1.
# Test Data : Rows = 3, Columns = 4
# Expected Result : [[0, 0, 0, 0], [0, 1, 2, 3], [0, 2, 4, 6]]
row_num = int(input("Input number of rows: "))
col_num = int(input("Input number of columns: "))
multi_list = [[0 for col in range(col_num)] for row in range(row_num)]
for row in range(row_num):
    for col in range(col_num):
        multi_list[row][col]= row*col

print(multi_list)

# 12. Write a Python program that accepts a sequence of lines (blank line to terminate) as input and prints the lines as output (all characters in lower case). 
lines = []
while True:
    l = input()
    if l:
        lines.append(l.upper())
    else:
        break
for l in lines:
    print(l)

# 13. Write a Python program which accepts a sequence of comma separated 4 digit binary numbers as its input and print the numbers that are divisible by 5 in a comma separated sequence. 
# Sample Data : 0100,0011,1010,1001,1100,1001
# Expected Output : 1010
items = []
num = [x for x in input().split(',')]
for p in num:
    x = int(p, 2)
    if not x % 5:
        items.append(p)
print(','.join(items))

# 14. Write a Python program that accepts a string and calculate the number of digits and letters. 
# Sample Data : Python 3.2
# Expected Output :
# Letters 6
# Digits 2
s = input("Input a string")
d = l = 0
for c in s:
    if c.isdigit():
        d = d+1
    elif c.isalpha():
        l = l+1
    else:
        pass
print("Letters", l)
print("Digits", d)

# 15. Write a Python program to check the validity of password input by users. 
# Validation :
#     At least 1 letter between [a-z] and 1 letter between [A-Z].
#     At least 1 number between [0-9].
#     At least 1 character from [$#@].
#     Minimum length 6 characters.
#     Maximum length 16 characters.
import re
p= input("Input your password")
x = True
while x:  
    if (len(p)<6 or len(p)>12):
        break
    elif not re.search("[a-z]",p):
        break
    elif not re.search("[0-9]",p):
        break
    elif not re.search("[A-Z]",p):
        break
    elif not re.search("[$#@]",p):
        break
    elif re.search("\s",p):
        break
    else:
        print("Valid Password")
        x=False
        break
if x:
    print("Not a Valid Password")


# 16. Write a Python program to find numbers between 100 and 400 (both included) where each digit of a number is an even number. The numbers obtained should be printed in a comma-separated sequence. 
items = []
for i in range(100, 401):
    s = str(i)
    if (int(s[0]) % 2 == 0) and (int(s[1]) % 2 == 0) and (int(s[2]) % 2 == 0):
        items.append(s)
print(",".join(items))

# 17. Write a Python program to print alphabet pattern 'A'. 
# Expected Output:
#   ***                                                                   
#  *   *                                                                  
#  *   *                                                                  
#  *****                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (((column == 1 or column == 5) and row != 0) or ((row == 0 or row == 3) and (column > 1 and column < 5))):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 18. Write a Python program to print alphabet pattern 'D'. 
# Expected Output:
#  ****                                                                   
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  **** 
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 1 or ((row == 0 or row == 6) and (column > 1 and column < 5)) or (column == 5 and row != 0 and row != 6)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 19. Write a Python program to print alphabet pattern 'E'. 
# Expected Output:
#  *****                                                                  
#  *                                                                      
#  *                                                                      
#  ****                                                                   
#  *                                                                      
#  *                                                                      
#  *****
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 1 or ((row == 0 or row == 6) and (column > 1 and column < 6)) or (row == 3 and column > 1 and column < 5)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 20. Write a Python program to print alphabet pattern 'G'. 
# Expected Output:
#   ***                                                                   
#  *   *                                                                  
#  *                                                                      
#  * ***                                                                  
#  *   *                                                                  
#  *   *                                                                  
#   *** 
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if ((column == 1 and row != 0 and row != 6) or ((row == 0 or row == 6) and column > 1 and column < 5) or (row == 3 and column > 2 and column < 6) or (column == 5 and row != 0 and row != 2 and row != 6)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 21. Write a Python program to print alphabet pattern 'L'. 
# Expected Output:
#  *                                                                      
#  *                                                                      
#  *                                                                      
#  *                                                                      
#  *                                                                      
#  *                                                                      
#  *****
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 1 or (row == 6 and column != 0 and column < 6)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 22. Write a Python program to print alphabet pattern 'M'. 
# Expected Output:
#   *       *                                                             
#   *       *                                                             
#   * *   * *                                                             
#   *   *   *                                                             
#   *       *                                                             
#   *       *                                                             
#   *       *
result_str="";    
for row in range(0,7):    
    for column in range(0,7):     
        if (column == 1 or column == 5 or (row == 2 and (column == 2 or column == 4)) or (row == 3 and column == 3)):  
            result_str=result_str+"* "    
        else:      
            result_str=result_str+"  "    
    result_str=result_str+"\n"    
print(result_str);

# 23. Write a Python program to print alphabet pattern 'O'. 
# Expected Output:
#   ***                                                                   
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#   *** 
result_str="";    
for row in range(0,7):    
    for column in range(0,7):     
        if (((column == 1 or column == 5) and row != 0 and row != 6) or ((row == 0 or row == 6) and column > 1 and column < 5)):  
            result_str=result_str+"*"    
        else:      
            result_str=result_str+" "    
    result_str=result_str+"\n"    
print(result_str);

# 24. Write a Python program to print alphabet pattern 'P'. 
# Expected Output:
#  ****                                                                   
#  *   *                                                                  
#  *   *                                                                  
#  ****                                                                   
#  *                                                                      
#  *                                                                      
#  *  
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 1 or ((row == 0 or row == 3) and column > 0 and column < 5) or ((column == 5 or column == 1) and (row == 1 or row == 2))):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 25. Write a Python program to print alphabet pattern 'R'. 
# Expected Output:
#  ****                                                                   
#  *   *                                                                  
#  *   *                                                                  
#  ****                                                                   
#  * *                                                                    
#  *  *                                                                   
#  *   *
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 1 or ((row == 0 or row == 3) and column > 1 and column < 5) or (column == 5 and row != 0 and row < 3) or (column == row - 1 and row > 2)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 26. Write a Python program to print the following patterns. 
# Expected Output:
#   ****                                                                  
#  *                                                                      
#  *                                                                      
#   ***                                                                   
#      *                                                                  
#      *                                                                  
#  **** 
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (((row == 0 or row == 3 or row == 6) and column > 1 and column < 5) or (column == 1 and (row == 1 or row == 2 or row == 6)) or (column == 5 and (row == 0 or row == 4 or row == 5))):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

row = 15
col = 18
result_str = ""
for i in range(1, row+1):
    if((i <= 3) or (i >= 7 and i <= 9) or (i >= 13 and i <= 15)):
        for j in range(1, col):
            result_str = result_str+"o"
        result_str = result_str+"\n"
    elif(i >= 4 and i <= 6):
        for j in range(1, 5):
            result_str = result_str+"o"
        result_str = result_str+"\n"
    else:
        for j in range(1, 14):
            result_str = result_str+" "
        for j in range(1, 5):
            result_str = result_str+"o"
        result_str = result_str+"\n"
print(result_str)

# 27. Write a Python program to print alphabet pattern 'T'. 
# Expected Output:
#  *****                                                                  
#    *                                                                    
#    *                                                                    
#    *                                                                    
#    *                                                                    
#    *                                                                    
#    *  
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (column == 3 or (row == 0 and column > 0 and column < 6)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 28. Write a Python program to print alphabet pattern 'U'. 
# Expected Output:
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#  *   *                                                                  
#   *** 
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (((column == 1 or column == 5) and row != 6) or (row == 6 and column > 1 and column < 5)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 29. Write a Python program to print alphabet pattern 'X'. 
# Expected Output:
#  *   *                                                                  
#  *   *                                                                  
#   * *                                                                   
#    *                                                                    
#   * *                                                                   
#  *   *                                                                  
#  *   *
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (((column == 1 or column == 5) and (row > 4 or row < 2)) or row == column and column > 0 and column < 6 or (column == 2 and row == 4) or (column == 4 and row == 2)):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 30. Write a Python program to print alphabet pattern 'Z'. 
# Expected Output:
# *******                                                                 
#      *                                                                  
#     *                                                                   
#    *                                                                    
#   *                                                                     
#  *                                                                      
# *******
result_str = ""
for row in range(0, 7):
    for column in range(0, 7):
        if (((row == 0 or row == 6) and column >= 0 and column <= 6) or row+column == 6):
            result_str = result_str+"*"
        else:
            result_str = result_str+" "
    result_str = result_str+"\n"
print(result_str)

# 31. Write a Python program to calculate a dog's age in dog's years. 
# Note: For the first two years, a dog year is equal to 10.5 human years. After that, each dog year equals 4 human years.
# Expected Output:
# Input a dog's age in human years: 15                                    
# The dog's age in dog's years is 73
h_age = int(input("Input a dog's age in human years: "))
if h_age < 0:
	print("Age must be positive number.")
	exit()
elif h_age <= 2:
	d_age = h_age * 10.5
else:
	d_age = 21 + (h_age - 2)*4

print("The dog's age in dog's years is", d_age)

# 32. Write a Python program to check whether an alphabet is a vowel or consonant. 
# Expected Output:
# Input a letter of the alphabet: k                                       
# k is a consonant.
l = input("Input a letter of the alphabet: ")

if l in ('a', 'e', 'i', 'o', 'u'):
	print("%s is a vowel." % l)
else:
	print("%s is a consonant." % l)

# 33. Write a Python program to convert month name to a number of days. 
# Expected Output:
# List of months: January, February, March, April, May, June, July, August
# , September, October, November, December                                
# Input the name of Month: February                                       
# No. of days: 28/29 days 
print("List of months: January, February, March, April, May, June, July, August, September, October, November, December")
month_name = input("Input the name of Month: ")

if month_name == "February":
	print("No. of days: 28/29 days")
elif month_name in ("April", "June", "September", "November"):
	print("No. of days: 30 days")
elif month_name in ("January", "March", "May", "July", "August", "October", "December"):
	print("No. of days: 31 day")
else:
	print("Wrong month name")

# 34. Write a Python program to sum of two given integers. However, if the sum is between 15 to 20 it will return 20. 
def sum(x, y):
    sum = x + y
    if sum in range(15, 20):
        return 20
    else:
        return sum
print(sum(10, 6))
print(sum(10, 2))
print(sum(10, 12))

# 35. Write a Python program to check a string represent an integer or not. 
# Expected Output:
# Input a string: Python                                                  
# The string is not an integer.  
text = input("Input a string: ")
text = text.strip()
if len(text) < 1:
    print("Input a valid text")
else:
    if all(text[i] in "0123456789" for i in range(len(text))):
        print("The string is an integer.")
    elif (text[0] in "+-") and \
            all(text[i] in "0123456789" for i in range(1, len(text))):
        print("The string is an integer.")
    else:
        print("The string is not an integer.")

# 36. Write a Python program to check a triangle is equilateral, isosceles or scalene. 
# Note :
# An equilateral triangle is a triangle in which all three sides are equal.
# A scalene triangle is a triangle that has three unequal sides.
# An isosceles triangle is a triangle with (at least) two equal sides.
# Expected Output:
# Input lengths of the triangle sides:                                    
# x: 6                                                                    
# y: 8                                                                    
# z: 12                                                                   
# Scalene triangle  
print("Input lengths of the triangle sides: ")
x = int(input("x: "))
y = int(input("y: "))
z = int(input("z: "))

if x == y == z:
	print("Equilateral triangle")
elif x == y or y == z or z == x:
	print("isosceles triangle")
else:
	print("Scalene triangle")

# 37. Write a Python program that reads two integers representing a month and day and prints the season for that month and day. 
# Expected Output:
# Input the month (e.g. January, February etc.): july                     
# Input the day: 31                                                       
# Season is autumn  
month = input("Input the month (e.g. January, February etc.): ")
day = int(input("Input the day: "))

if month in ('January', 'February', 'March'):
	season = 'winter'
elif month in ('April', 'May', 'June'):
	season = 'spring'
elif month in ('July', 'August', 'September'):
	season = 'summer'
else:
	season = 'autumn'

if (month == 'March') and (day > 19):
	season = 'spring'
elif (month == 'June') and (day > 20):
	season = 'summer'
elif (month == 'September') and (day > 21):
	season = 'autumn'
elif (month == 'December') and (day > 20):
	season = 'winter'
print("Season is", season)

# 38. Write a Python program to display astrological sign for given date of birth. 
# Expected Output:
# Input birthday: 15                                                      
# Input month of birth (e.g. march, july etc): may                        
# Your Astrological sign is : Taurus 
day = int(input("Input birthday: "))
month = input("Input month of birth (e.g. march, july etc): ")
if month == 'december':
	astro_sign = 'Sagittarius' if (day < 22) else 'capricorn'
elif month == 'january':
	astro_sign = 'Capricorn' if (day < 20) else 'aquarius'
elif month == 'february':
	astro_sign = 'Aquarius' if (day < 19) else 'pisces'
elif month == 'march':
	astro_sign = 'Pisces' if (day < 21) else 'aries'
elif month == 'april':
	astro_sign = 'Aries' if (day < 20) else 'taurus'
elif month == 'may':
	astro_sign = 'Taurus' if (day < 21) else 'gemini'
elif month == 'june':
	astro_sign = 'Gemini' if (day < 21) else 'cancer'
elif month == 'july':
	astro_sign = 'Cancer' if (day < 23) else 'leo'
elif month == 'august':
	astro_sign = 'Leo' if (day < 23) else 'virgo'
elif month == 'september':
	astro_sign = 'Virgo' if (day < 23) else 'libra'
elif month == 'october':
	astro_sign = 'Libra' if (day < 23) else 'scorpio'
elif month == 'november':
	astro_sign = 'scorpio' if (day < 22) else 'sagittarius'
print("Your Astrological sign is :", astro_sign)

# 39. Write a Python program to display the sign of the Chinese Zodiac for given year in which you were born. 
# Expected Output:
# Input your birth year: 1973                                             
# Your Zodiac sign : Ox  
year = int(input("Input your birth year: "))
if (year - 2000) % 12 == 0:
    sign = 'Dragon'
elif (year - 2000) % 12 == 1:
    sign = 'Snake'
elif (year - 2000) % 12 == 2:
    sign = 'Horse'
elif (year - 2000) % 12 == 3:
    sign = 'sheep'
elif (year - 2000) % 12 == 4:
    sign = 'Monkey'
elif (year - 2000) % 12 == 5:
    sign = 'Rooster'
elif (year - 2000) % 12 == 6:
    sign = 'Dog'
elif (year - 2000) % 12 == 7:
    sign = 'Pig'
elif (year - 2000) % 12 == 8:
    sign = 'Rat'
elif (year - 2000) % 12 == 9:
    sign = 'Ox'
elif (year - 2000) % 12 == 10:
    sign = 'Tiger'
else:
    sign = 'Hare'
print("Your Zodiac sign :",sign)


# 40. Write a Python program to find the median of three values. 
# Expected Output:
# Input first number: 15                                                  
# Input second number: 26                                                 
# Input third number: 29                                                  
# The median is 26.0   
a = float(input("Input first number: "))
b = float(input("Input second number: "))
c = float(input("Input third number: "))
if a > b:
    if a < c:
        median = a
    elif b > c:
        median = b
    else:
        median = c
else:
    if a > c:
        median = a
    elif b < c:
        median = b
    else:
        median = c
print("The median is", median)

# 41. Write a Python program to get next day of a given date. 
# Expected Output:
# Input a year: 2016                                                      
# Input a month [1-12]: 08                                                
# Input a day [1-31]: 23                                                  
# The next date is [yyyy-mm-dd] 2016-8-24   
year = int(input("Input a year: "))

if (year % 400 == 0):
    leap_year = True
elif (year % 100 == 0):
    leap_year = False
elif (year % 4 == 0):
    leap_year = True
else:
    leap_year = False

month = int(input("Input a month [1-12]: "))

if month in (1, 3, 5, 7, 8, 10, 12):
    month_length = 31
elif month == 2:
    if leap_year:
        month_length = 29
    else:
        month_length = 28
else:
    month_length = 30
day = int(input("Input a day [1-31]: "))
if day < month_length:
    day += 1
else:
    day = 1
    if month == 12:
        month = 1
        year += 1
    else:
        month += 1
print("The next date is [yyyy-mm-dd] %d-%d-%d." % (year, month, day))

# 42. Write a Python program to calculate the sum and average of n integer numbers (input from the user). Input 0 to finish. 
print("Input some integers to calculate their sum and average. Input 0 to exit.")
count = 0
sum = 0.0
number = 1
while number != 0:
	number = int(input(""))
	sum = sum + number
	count += 1
if count == 0:
	print("Input some numbers")
else:
	print("Average and Sum of the above numbers are: ", sum / (count-1), sum)

# 43. Write a Python program to create the multiplication table (from 1 to 10) of a number. 
# Expected Output:
# Input a number: 6                                                       
# 6 x 1 = 6                                                               
# 6 x 2 = 12                                                              
# 6 x 3 = 18                                                              
# 6 x 4 = 24                                                              
# 6 x 5 = 30                                                              
# 6 x 6 = 36                                                              
# 6 x 7 = 42                                                              
# 6 x 8 = 48                                                              
# 6 x 9 = 54                                                              
# 6 x 10 = 60 
n = int(input("Input a number: "))
# use for loop to iterate 10 times
for i in range(1,11):
   print(n,'x',i,'=',n*i)

# 44. Write a Python program to construct the following pattern, using a nested loop number. 
# Expected Output:
# 1
# 22
# 333
# 4444
# 55555
# 666666
# 7777777
# 88888888
# 999999999
for i in range(10):
    print(str(i) * i)




CSV.py # 1. Write a Python program to read each row from a given csv file and print a list of strings.

import sys
import csv
with open('departments.csv', newline='') as csvfile:
  data = csv.reader(csvfile, delimiter=' ', quotechar='|')
  for row in data:
    print(', '.join(row))

'''
department_id,department_name,manager_id,location_id
10,Administration,200,1700
20,Marketing,201,1800
30,Purchasing,114,1700
40,Human Resources,203,2400
50,Shipping,121,1500
60,IT,103,1400
70,Public Relations,204,2700
80,Sales,145,2500
90,Executive,100,1700
100,Finance,108,1700
110,Accounting,205,1700
120,Treasury,,1700
130,Corporate Tax,,1700
140,Control And Credit,,1700
150,Shareholder Services,,1700
160,Benefits,,1700
170,Manufacturing,,1700
180,Construction,,1700
190,Contracting,,1700
200,Operations,,1700
210,IT Support,,1700
220,NOC,,1700
230,IT Helpdesk,,1700
240,Government Sales,,1700
250,Retail Sales,,1700
260,Recruiting,,1700
270,Payroll,,1700
'''

# 2. Write a Python program to read a given CSV file having tab delimiter.
import csv
with open('countries.csv', newline='') as csvfile:
  data = csv.reader(csvfile, delimiter='\t')
  for row in data:
    print(', '.join(row))
'''
country_id country_name region_id
AR Argentina 2
AU Australia 3
BE Belgium 1
BR Brazil 2
CA Canada 2
CH Switzerland 1
CN China 3
DE Germany 1
DK Denmark 1
EG Egypt 4
FR France 1
HK HongKong 3
IL Israel 4
IN India 3
IT Italy 1
JP Japan 3
KW Kuwait 4
MX Mexico 2
NG Nigeria 4
N Netherlands 1
SG Singapore 3
UK United Kingdom 1
US United States of America 2
ZM Zambia 4
ZW Zimbabwe 4
'''

# 3. Write a Python program to read a given CSV file as a list.
import csv
with open('employees.csv', newline='') as f:
  reader = csv.reader(f)
  data_list = list(reader)
print(data_list)
'''
employ_id,first_name,last_name,email,phone_number,hire_date,job_id,salary,commission_pct,manager_id,department_id
100,Steven,King,SKING,515.123.4567,1987-06-17,AD_PRES,24000,,,90
101,Neena,Kochhar,NKOCHHAR,515.123.4568,1987-06-18,AD_VP,17000,,100,90
102,Lex,De Haan,LDEHAAN,515.123.4569,1987-06-19,AD_VP,17000,,100,90
103,Alexander,Hunold,AHUNOLD,590.423.4567,1987-06-20,IT_PROG,9000,,102,60
104,Bruce,Ernst,BERNST,590.423.4568,1987-06-21,IT_PROG,6000,,103,60
105,David,Austin,DAUSTIN,590.423.4569,1987-06-22,IT_PROG,4800,,103,60
106,Valli,Pataballa,VPATABAL,590.423.4560,1987-06-23,IT_PROG,4800,,103,60
107,Diana,Lorentz,DLORENTZ,590.423.5567,1987-06-24,IT_PROG,4200,,103,60
108,Nancy,Greenberg,NGREENBE,515.124.4569,1987-06-25,FI_MGR,12000,,101,100
109,Daniel,Faviet,DFAVIET,515.124.4169,1987-06-26,FI_ACCOUNT,9000,,108,100
110,John,Chen,JCHEN,515.124.4269,1987-06-27,FI_ACCOUNT,8200,,108,100
111,Ismael,Sciarra,ISCIARRA,515.124.4369,1987-06-28,FI_ACCOUNT,7700,,108,100
112,Jose Manuel,Urman,JMURMAN,515.124.4469,1987-06-29,FI_ACCOUNT,7800,,108,100
113,Luis,Popp,LPOPP,515.124.4567,1987-06-30,FI_ACCOUNT,6900,,108,100
114,Den,Raphaely,DRAPHEAL,515.127.4561,1987-07-01,PU_MAN,11000,,100,30
115,Alexander,Khoo,AKHOO,515.127.4562,1987-07-02,PU_CLERK,3100,,114,30
116,Shelli,Baida,SBAIDA,515.127.4563,1987-07-03,PU_CLERK,2900,,114,30
117,Sigal,Tobias,STOBIAS,515.127.4564,1987-07-04,PU_CLERK,2800,,114,30
118,Guy,Himuro,GHIMURO,515.127.4565,1987-07-05,PU_CLERK,2600,,114,30
119,Karen,Colmenares,KCOLMENA,515.127.4566,1987-07-06,PU_CLERK,2500,,114,30
120,Matthew,Weiss,MWEISS,650.123.1234,1987-07-07,ST_MAN,8000,,100,50
121,Adam,Fripp,AFRIPP,650.123.2234,1987-07-08,ST_MAN,8200,,100,50
122,Payam,Kaufling,PKAUFLIN,650.123.3234,1987-07-09,ST_MAN,7900,,100,50
123,Shanta,Vollman,SVOLLMAN,650.123.4234,1987-07-10,ST_MAN,6500,,100,50
124,Kevin,Mourgos,KMOURGOS,650.123.5234,1987-07-11,ST_MAN,5800,,100,50
125,Julia,Nayer,JNAYER,650.124.1214,1987-07-12,ST_CLERK,3200,,120,50
126,Irene,Mikkilineni,IMIKKILI,650.124.1224,1987-07-13,ST_CLERK,2700,,120,50
127,James,Landry,JLANDRY,650.124.1334,1987-07-14,ST_CLERK,2400,,120,50
128,Steven,Markle,SMARKLE,650.124.1434,1987-07-15,ST_CLERK,2200,,120,50
129,Laura,Bissot,LBISSOT,650.124.5234,1987-07-16,ST_CLERK,3300,,121,50
130,Mozhe,Atkinson,MATKINSO,650.124.6234,1987-07-17,ST_CLERK,2800,,121,50
131,James,Marlow,JAMRLOW,650.124.7234,1987-07-18,ST_CLERK,2500,,121,50
132,TJ,Olson,TJOLSON,650.124.8234,1987-07-19,ST_CLERK,2100,,121,50
133,Jason,Mallin,JMALLIN,650.127.1934,1987-07-20,ST_CLERK,3300,,122,50
134,Michael,Rogers,MROGERS,650.127.1834,1987-07-21,ST_CLERK,2900,,122,50
135,Ki,Gee,KGEE,650.127.1734,1987-07-22,ST_CLERK,2400,,122,50
136,Hazel,Philtanker,HPHILTAN,650.127.1634,1987-07-23,ST_CLERK,2200,,122,50
137,Renske,Ladwig,RLADWIG,650.121.1234,1987-07-24,ST_CLERK,3600,,123,50
138,Stephen,Stiles,SSTILES,650.121.2034,1987-07-25,ST_CLERK,3200,,123,50
139,John,Seo,JSEO,650.121.2019,1987-07-26,ST_CLERK,2700,,123,50
140,Joshua,Patel,JPATEL,650.121.1834,1987-07-27,ST_CLERK,2500,,123,50
141,Trenna,Rajs,TRAJS,650.121.8009,1987-07-28,ST_CLERK,3500,,124,50
142,Curtis,Davies,CDAVIES,650.121.2994,1987-07-29,ST_CLERK,3100,,124,50
143,Randall,Matos,RMATOS,650.121.2874,1987-07-30,ST_CLERK,2600,,124,50
144,Peter,Vargas,PVARGAS,650.121.2004,1987-07-31,ST_CLERK,2500,,124,50
145,John,Russell,JRUSSEL,011.44.1344.429268,1987-08-01,SA_MAN,14000,0.4,100,80
146,Karen,Partners,KPARTNER,011.44.1344.467268,1987-08-02,SA_MAN,13500,0.3,100,80
147,Alberto,Errazuriz,AERRAZUR,011.44.1344.429278,1987-08-03,SA_MAN,12000,0.3,100,80
148,Gerald,Cambrault,GCAMBRAU,011.44.1344.619268,1987-08-04,SA_MAN,11000,0.3,100,80
149,Eleni,Zlotkey,EZLOTKEY,011.44.1344.429018,1987-08-05,SA_MAN,10500,0.2,100,80
150,Peter,Tucker,PTUCKER,011.44.1344.129268,1987-08-06,SA_REP,10000,0.3,145,80
'''

# 4. Write a Python program to read a given CSV file as a dictionary.
data = csv.DictReader(open("departments.csv"))
print("CSV file as a dictionary:\n")
for row in data:
  print(row)
'''
department_id, department_name,  manager_id,  location_id
10, Administration, 200, 1700
20, Marketing, 201, 1800
30, Purchasing, 114, 1700
40, Human Resources, 203, 2400
50, Shipping, 121, 1500
60, IT, 103, 1400
70, Public Relations, 204, 2700
80, Sales, 145, 2500
'''

# 5. Write a Python program to read a given CSV files with initial spaces after a delimiter and remove those initial spaces.
print("\nWith initial spaces after a delimiter:\n")
with open('departments.csv', 'r') as csvfile:
  data = csv.reader(csvfile, skipinitialspace=False)
  for row in data:
    print(', '.join(row))
print("\n\nWithout initial spaces after a delimiter:\n")
with open('departments.csv', 'r') as csvfile:
  data = csv.reader(csvfile, skipinitialspace=True)
  for row in data:
    print(', '.join(row))
'''
CSV REFER DEPTARTMENT
'''

# 6. Write a Python program that reads a CSV file and remove initial spaces, quotes around each entry and the delimiter.
csv.register_dialect('csv_dialect',
                     delimiter='|',
                     skipinitialspace=True,
                     quoting=csv.QUOTE_ALL)
with open('temp.csv', 'r') as csvfile:
  reader = csv.reader(csvfile, dialect='csv_dialect')
  for row in reader:
    print(row)
'''
"country_id"|"country_name"|"region_id"
"AR"|"Argentina"| 2
"AU"|"Australia"| 3
"BE"|"Belgium"| 1
"BR"|"Brazil"| 2
"CA"|"Canada"| 2

'''

# 7. Write a Python program to read specific columns of a given CSV file and print the content of the columns.

with open('departments.csv', newline='') as csvfile:
  data = csv.DictReader(csvfile)
  print("ID Department Name")
  print("---------------------------------")
  for row in data:
    print(row['department_id'], row['department_name'])
'''
department_id,department_name,manager_id,location_id
10,Administration,200,1700
20,Marketing,201,1800
30,Purchasing,114,1700
40,Human Resources,203,2400
50,Shipping,121,1500
60,IT,103,1400
70,Public Relations,204,2700
80,Sales,145,2500
90,Executive,100,1700
100,Finance,108,1700
110,Accounting,205,1700
120,Treasury,,1700
130,Corporate Tax,,1700
140,Control And Credit,,1700
150,Shareholder Services,,1700
160,Benefits,,1700
170,Manufacturing,,1700
180,Construction,,1700
190,Contracting,,1700
200,Operations,,1700
210,IT Support,,1700
220,NOC,,1700
230,IT Helpdesk,,1700
240,Government Sales,,1700
250,Retail Sales,,1700
260,Recruiting,,1700
270,Payroll,,1700
'''

# 8. Write a Python program that reads each row of a given csv file and skip the header of the file. Also print the number of rows and the field names.
fields = []
rows = []
with open('departments.csv', newline='') as csvfile:
  data = csv.reader(csvfile, delimiter=' ', quotechar=',')
  # Following command skips the first row of the CSV file.
  fields = next(data)
  for row in data:
    print(', '.join(row))
print("\nTotal no. of rows: %d" % (data.line_num))
print('Field names are:')
print(', '.join(fields))
'''
CSV FILE SAME AS ABOVE
'''

# 9. Write a Python program to create an object for writing and iterate over the rows to print the values.
with open('temp.csv', 'wt') as f:
  writer = csv.writer(f)
  writer.writerow(('id1', 'id2', 'date'))
  for i in range(3):
    row = (
        i + 1,
        chr(ord('a') + i),
        '01/{:02d}/2019'.format(i + 1),)
    writer.writerow(row)
print(open('temp.csv', 'rt').read())

'''
"country_id"|"country_name"|"region_id"
"AR"|"Argentina"| 2
"AU"|"Australia"| 3
"BE"|"Belgium"| 1
"BR"|"Brazil"| 2
"CA"|"Canada"| 2

'''

# 10. Write a Python program to write a Python list of lists to a csv file. After writing the CSV file read the CSV file and display the content.
data = [[10, 'a1', 1], [12, 'a2', 3], [
    14, 'a3', 5], [16, 'a4', 7], [18, 'a5', 9]]
with open("temp.csv", "w", newline="") as f:
  writer = csv.writer(f)
  writer.writerows(data)
with open('temp.csv', newline='') as csvfile:
  data = csv.reader(csvfile, delimiter=' ')
  for row in data:
    print(', '.join(row))
'''
CSV SAME AS ABOVE
'''

# 11. Write a Python program to write a Python dictionary to a csv file. After writing the CSV file read the CSV file and display the content.
csv_columns = ['id', 'Column1', 'Column2', 'Column3', 'Column4', 'Column5']
dict_data = {'id': ['1', '2', '3'],
             'Column1': [33, 25, 56],
             'Column2': [35, 30, 30],
             'Column3': [21, 40, 55],
             'Column4': [71, 25, 55],
             'Column5': [10, 10, 40], }
csv_file = "temp.csv"
try:
  with open(csv_file, 'w') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=csv_columns)
    writer.writeheader()
    for data in dict_data:
      writer.writerow(dict_data)
except IOError:
  print("I/O error")
data = csv.DictReader(open(csv_file))
print("CSV file as a dictionary:\n")
for row in data:
  print(row)
'''
CSV SAME AS ABOVE
'''




DataStructures.py """
1. Write a Python program to create an Enum object and display a member name
and value.
Sample data :
Afghanistan = 93
Albania = 355
Algeria = 213
Andorra = 376
Angola = 244
Antarctica = 672
Expected Output :
Member name: Albania
Member value: 355
"""


from enum import Enum

class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672

##print(Country.Albania)
##print(repr(Country.Albania))
##print(type(Country.Albania))
##print(isinstance(Country.Albania, Country))
##print(Country['Albania'])
##print(Country['Antarctica'].value)
##for country in Country:
##    print(country)
print('Member name:', Country.Albania.name)
print('Member value:', Country.Albania.value)

"""
2. Write a Python program to iterate over an enum class and display individual
member and their value. 
Expected Output:
Afghanistan = 93
Albania = 355
Algeria = 213
Andorra = 376
Angola = 244
Antarctica = 672
"""

from enum import Enum

class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672

for country in Country:
    print('Member name is {:<11}; member value is {}'.format(country.name, country.value))

"""
3. Write a Python program to display all the member name of an enum class
ordered by their values. 
Expected Output:
Country Name ordered by Country Code:
Afghanistan
Algeria
Angola
Albania
Andorra
Antarctica
"""
# solution 1
from enum import Enum

class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672   

print('Country Name ordered by Country Code:')
for country in sorted(Country, key=lambda x: x.value):
    print(country.name)

# solution 2 
from enum import IntEnum

class Country(IntEnum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672   

print('Country Name ordered by Country Code:')
for country in sorted(Country):
    print(country.name)

"""
4. Write a Python program to get all values from an enum class. 
Expected output:
[93, 355, 213, 376, 244, 672]
"""

from enum import Enum

class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672   

output = [country.value for country in Country]
print(output)

# alternatively:
output = [country.value for country in Country]
print(output)

"""
5. Write a Python program to count the most common words in a dictionary. 
Expected Output:
[('pink', 6), ('black', 5), ('white', 5), ('red', 4)]
"""

words = ['red', 'green', 'black', 'pink', 'black', 'white', 'black', 'eyes',
         'white', 'black', 'orange', 'pink', 'pink', 'red', 'red', 'white',
         'orange', 'white', 'black', 'pink', 'green', 'green', 'pink', 'green',
         'white', 'orange', "orange", 'red']

counts = {(word, words.count(word)) for word in words}
output = sorted(counts, key=lambda x: x[1], reverse=True)
print(output[:4])

# solution 2 with collections
from collections import Counter
print(Counter(words).most_common(4))

"""
6. Write a Python program to find the class wise roll number from a
tuple-of-tuples.
Sample data:
classes = (
('V', 1),
('VI', 1),
('V', 2),
('VI', 2),
('VI', 3),
('VII', 1),
)
Expected Output:
defaultdict(, {'VII': [1], 'V': [1, 2], 'VI': [1, 2, 3]})
"""

classes = (('V', 1),
           ('VI', 1),
           ('V', 2),
           ('VI', 2),
           ('VI', 3),
           ('VII', 1))

my_dict = {}
for a,b in classes:
    my_dict.setdefault(a, []).append(b)
print(my_dict)

"""
7. Write a Python program to count the number of students of individual class.
Sample data:
classes = (
('V', 1),
('VI', 1),
('V', 2),
('VI', 2),
('VI', 3),
('VII', 1),
)
Expected Output:
Counter({'VI': 3, 'V': 2, 'VII': 1})
"""

classes = (('V', 1),
           ('VI', 1),
           ('V', 2),
           ('VI', 2),
           ('VI', 3),
           ('VII', 1))
# solution 1 (if numbers are sequentials and start with 1)
my_dict = {}
for a, b in classes:
    if a in my_dict and b > my_dict[a] or a not in my_dict:
        my_dict[a] = b
print(my_dict)

only_classes, roll_numbers = zip(*classes)
my_dict = {a: only_classes.count(a) for a in only_classes}
print(my_dict)

"""
8.Write a Python program to get the unique enumeration values. 
Expected Output:
Afghanistan = 93
Albania = 355
Algeria = 213
Andorra = 376
Angola = 244
"""
from enum import Enum, unique
@unique
class Country(Enum):

    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244

for c in Country:
    print(c.name, c.value)

"""
9. Write a Python program to create an instance of an OrderedDict using a given
dictionary. Sort the dictionary during the creation and print the members of
the dictionary in reverse order. 
Expected Output: 
Angola 244
Andorra 376
Algeria 213
Afghanistan 93
Albania 355
In reverse order:
Albania 355
Afghanistan 93
Algeria 213
Andorra 376
Angola 244
"""

from collections import OrderedDict

my_dict = dict(Afghanistan = 93,
               Albania = 355,
               Algeria = 213,
               Andorra = 376,
               Angola = 244)
# OrderedDict sorted by values in ascending order
my_ordered_dict = OrderedDict(sorted(my_dict.items(), key = lambda x: x[1]))
for k,v in my_ordered_dict.items():
    print(k, v)

# OrderedDict sorted by values in descending (reverse) order
print("In reverse order:")
reversed_dict = OrderedDict(sorted(my_dict.items(), key=lambda x: x[1], reverse=True))
for k,v in reversed_dict.items():
    print(k, v)

# or alternatively reversing without creating additional OrderedDict
print("In reverse order:")
for k in reversed(my_ordered_dict):
    print(k, my_ordered_dict[k])


"""
10. Write a Python program to group a sequence of key-value pairs into a
dictionary of lists.
Example:
[('v', 1), ('vi', 2), ('v', 3), ('vi', 4), ('vii', 1)]
Expected output:
[('v', [1, 3]), ('vi', [2, 4]), ('vii', [1])]
"""

classes = [('v', 1), ('vi', 2), ('v', 3), ('vi', 4), ('vii', 1)]

my_dict = {}
for k,v in classes:
    my_dict.setdefault(k, []).append(v)
print(my_dict)

"""
11. Write a Python program to compare two unordered lists (not sets).
Expected Output: False
"""
from collections import Counter

list_a = [1, 2, 5, 3, 4, 1, 2, 3]
list_b = [1, 3, 5, 2, 4, 3, 1, 2]

print(Counter(list_a) == Counter(list_b))

"""
12. Write a Python program to create an array contains six integers.
Also print all the members of the array. 
Expected Output:
10
20
30
40
50
"""

import array

my_arr = array.array('i', [1, 2, 3, 4, 5, 6])
print(my_arr)

for i in my_arr:
    print(i)

"""
13. Write a Python program to get the array size of types unsigned integer
and float. 
Expected Output:
4 
4
"""

import array

my_arr = array.array('I', [4, 4])
print(my_arr.itemsize)
my_arr = array.array('f', [4, 4])
print(my_arr.itemsize)

"""
14. Write a Python program to get an array buffer information. 
Expected Output:
Array buffer start address in memory and number of elements.
(25855056, 2)
"""
import array

my_arr = array.array('I', [4, 4, 4])
print(my_arr.buffer_info())
"""
15. Write a Python program to get the length of an array. 
Expected Output:
Length of the array is: 
5
"""

import array

my_arr = array.array('I', [4, 4, 4, 5])

# solution 1
print(my_arr.buffer_info()[1])

# solution 2
print(len(my_arr))

"""
16. Write a Python program to convert an array to an ordinary list with the
same items. 
Expected Output:
Original array:
array('b', [1, 2, 3, 4])
Array to list:
[1, 2, 3, 4] 
"""

import array

my_arr = array.array('b', [1, 2, 3, 4])

# solution 1
my_list = list(my_arr)
print(my_list, type(my_list))

# solution 2
my_list1 = my_arr.tolist()
print(my_list1, type(my_list1))

"""
17. Write a Python program to convert an array to an array of machine values
and return the bytes representation. 
Expected Output:
Original array: 
A1: array('i', [1, 2, 3, 4, 5, 6])
Array of bytes: b'010000000200000003000000040000000500000006000000'
"""

import array
import binascii

A1 = array.array('i', [1, 2, 3, 4, 5, 6])
print(binascii.b2a_hex(A1.tobytes()))

"""
18. Write a Python program to read a string and interpreting the string as
an array of machine values. 
Expected Output:
array1: array('i', [7, 8, 9, 10])
Bytes: b'0700000008000000090000000a000000' 
array2: array('i', [7, 8, 9, 10])
"""

import array

A1 = array.array('i', [1, 2, 3, 4, 5, 6])
A2 = array.array('i')
A2.frombytes(A1.tobytes())
print(A2)

"""
19. Write a Python program to push three items into the heap and print
the items from the heap. 
Expected Output:
('V', 1)
('V', 2)
('V', 3)
"""
import heapq

my_heap = []
heapq.heappush(my_heap, ('V', 1))
heapq.heappush(my_heap, ('V', 2))
heapq.heappush(my_heap, ('V', 3))

for i in my_heap:
    print(i)

"""
20. Write a Python program to push three items into a heap and return the
smallest item from the heap. Also Pop and return the smallest item from the heap.
Expected Output:
Items in the heap:
('V', 1)
('V', 3)
('V', 2) 
---------------------- 
The smallest item in the heap:
('V', 1) 
----------------------
Pop the smallest item in the heap:
('V', 2) 
('V', 3)
"""

import heapq

my_heap = []
heapq.heappush(my_heap, ('V', 0))
heapq.heappush(my_heap, ('V', 1))
heapq.heappush(my_heap, ('V', 1))

print("Items in the heap")
for i in my_heap:
    print(i)

print("The smallest item in the heap:")
print(*heapq.nsmallest(1, my_heap, key=lambda x: x[1]))
# also like this:
print(my_heap[0])

print("The smallest item in the heap:")
print(heapq.heappop(my_heap))

"""
21. Write a Python program to push an item on the heap, then pop and return
the smallest item from the heap. 
Expected Output:
Items in the heap:
('V', 1) 
('V', 3) 
('V', 2) 
----------------------
Using heappushpop push item on the heap and return the smallest item.
('V', 2) 
('V', 3) 
('V', 6)
"""

import heapq
my_heap = [('V', 1), ('V', 1)]

print(heapq.heappushpop(my_heap, ('V', 1)))
print(heapq.heappushpop(my_heap, ('V', 2)))
print(heapq.heappushpop(my_heap, ('V', 3)))

"""
22. Write a Python program to create a heapsort, pushing all values onto
a heap and then popping off the smallest values one at a time. 
Expected Output:
[10, 20, 20, 40, 50, 50, 60, 70, 80, 90, 100]
"""
import heapq
my_unsorted_list = [20, 10, 50, 70, 30, 10, 40, 80, 20, 60]
my_heap = []
for item in my_unsorted_list:
    heapq.heappush(my_heap, item)

my_sorted_list = [heapq.heappop(my_heap) for _ in range(len(my_heap))]
print(my_sorted_list)

"""
23. Write a Python program to get the two largest and three smallest items
from a dataset.
Expected Output:
[100, 90]
[10, 20, 20]
"""

import heapq

my_list = [20, 10, 50, 70, 30, 10, 40, 80, 20, 60]

print(heapq.nlargest(2, my_list))
print(heapq.nsmallest(3, my_list))

"""
24. Write a Python program to locate the left insertion point for a specified
value in sorted order. 
"""
import bisect

my_list = [10, 10, 20, 20, 30, 50, 40, 80, 70, 60]
print(bisect.bisect_left(my_list, 25))

"""
25. Write a Python program to locate the right insertion point for a specified
value in sorted order.
"""

import bisect

my_list = [10, 10, 20, 20, 30, 50, 40, 80, 70, 60]
print(bisect.bisect_right(my_list, 20))

"""
26. Write a Python program to insert items into a list in sorted order. 
Expected Output:
Original List: 
[25, 45, 36, 47, 69, 48, 68, 78, 14, 36] 
Sorted List: 
[14, 25, 36, 36, 45, 47, 48, 68, 69, 78]
"""
import bisect

my_list = [25, 45, 36, 47, 69, 48, 68, 78, 14, 36]
my_sorted_list = []
for i in my_list:
    bisect.insort_left(my_sorted_list, i)
print(my_sorted_list)
"""
27. Write a Python program to create a queue and display all the members and
size of the queue. 
Expected Output:
Members of the queue:
0 1 2 3 
Size of the queue:
4
"""
import queue
q = queue.Queue()
for i in range(4):
    q.put(i)

print("Members of the queue:")
for elem in list(q.queue):
    print(elem, end=' ')
print("\nSize of the queue:")
print(q.qsize())

"""
28. Write a Python program to find whether a queue is empty or not. Go to the editor
Expected Output:
True 
False
"""
import queue
q = queue.Queue()

if q.empty():
    print(True)
else:
    print(False)

"""
29. Write a Python program to create a FIFO queue. 
Expected Output:
0 1 2 3 
"""
import queue
q = queue.Queue()

for i in range(4):
    q.put(i)
while not q.empty():
    print(q.get(), end=' ')
print('\n')

"""
30. Write a Python program to create a LIFO queue. 
Expected Output:
3 2 1 0
"""

import queue

p = queue.LifoQueue()

for i in range(4):
    p.put(i)
while not p.empty():
    print(p.get(), end=' ')
print('\n')




Datetime.py """
1. Write a Python script to display the - 
a) Current date and time
b) Current year
c) Month of year
d) Week number of the year
e) Weekday of the week
f) Day of year
g) Day of the month
h) Day of week
"""

import datetime

print('current date:')
print(datetime.date.today())
print('current date and time:')
print(datetime.datetime.now())
print('current year:')
print(datetime.date.today().year)
print('current month:')
print(datetime.date.today().month)
print('current month name:')
print(datetime.date.today().strftime('%B'))
print('current week of year:')
print(datetime.date.today().isocalendar()[1])
print('current weekday of the week:')
print(datetime.date.today().isoweekday())    # MON is 1
print('current day of year:')
print(datetime.date.today().strftime('%j'))
print('current day of month:')
print(datetime.date.today().strftime('%d'))
print('current day of week:')
print(datetime.date.today().strftime('%A'))

"""
2. determine whether a given year is a leap year.
"""
import calendar

print(calendar.isleap(2018))

"""
3. convert a string to datetime.
Sample String : Jan 1 2014 2:43PM 
Expected Output : 2014-01-01 14:43:00
"""
import datetime

day = 'Jan 1 2014 2:43PM'
print(datetime.datetime.strptime(day, '%b %d %Y %I:%M%p'))

"""
4. get the current time in Python. 
Sample Format :  13:19:49.078205
"""
import datetime

print(datetime.datetime.now().time())

"""
5. subtract five days from current date.
Sample Date : 
Current Date : 2015-06-22
5 days before Current Date : 2015-06-17
"""
import datetime

print(datetime.date.today())
print(datetime.date.today()-datetime.timedelta(5))

"""
6. convert unix timestamp string to readable date.
Sample Unix timestamp string : 1284105682
Expected Output : 2010-09-10 13:31:22
"""
import datetime
utimestamp = 1284105682
print(datetime.datetime.fromtimestamp(utimestamp))

"""
7. print yesterday, today, tomorrow.
"""
import datetime

print(datetime.date.today()-datetime.timedelta(1))
print(datetime.date.today())
print(datetime.date.today()+datetime.timedelta(1))

"""
8. convert the date to datetime (midnight of the date)
in Python. 
Sample Output : 2015-06-22 00:00:00
"""
import datetime

a_date = datetime.date.today()
print(a_date)

# for printing only format as string:
formatted_date = a_date.__format__('%Y-%m-%d %H:%M:%S')
print(formatted_date)

# if used farther as datetime object:
formatted_date = datetime.datetime.combine(a_date, datetime.time.min)
print(formatted_date)

"""
9. print next 5 days starting from today.
"""
import datetime

for i in range(6):
    print(datetime.date.today()+datetime.timedelta(i))

"""
10. add 5 seconds with the current time.
Sample Data :
13:28:32.953088 
13:28:37.953088
"""
import datetime

t = datetime.time(13,28,32)
print(t)
dt = datetime.datetime.combine(datetime.date.today(), t)
dt = dt + datetime.timedelta(seconds=5)
print(dt.time())

"""
11. convert Year/Month/Day to Day of Year in Python.
"""
import datetime

my_date_str = '2018/04/18'
my_datetime = datetime.datetime.strptime(my_date_str, '%Y/%m/%d')
print(my_datetime)
print(my_datetime.strftime('%j'))

"""
12. get current time in milliseconds in Python
"""
import time

x = time.localtime()
milisecs = ((x.tm_hour * 60 + x.tm_min)*60 + x.tm_sec)*1000
print(milisecs)

"""
13. get week number. 
Sample Date : 2015, 6, 16
Expected Output : 25
"""
import datetime

td = datetime.date.today()
print(td.strftime('%W'))

"""
14. find the date of the first Monday of a given week.
Sample Year and week : 2015, 50
Expected Output : Mon Dec 14 00:00:00 2015
"""
import datetime

week = '50'
year = '2015'
first_mon = '1'
date = datetime.datetime.strptime(year + week + first_mon, '%Y%W%w')
print(date)

"""
15. select all the Sundays of a specified year.
"""
import datetime

year = '2018'
for week in range(53):
    print(datetime.datetime.strptime(year+str(week)+'0', '%Y%W%w'))


# 16. add year(s) with a given date and display the new date.

# Sample Data : (addYears is the user defined function name)
# print(addYears(datetime.date(2015,1,1), -1))
# print(addYears(datetime.date(2015,1,1), 0))
# print(addYears(datetime.date(2015,1,1), 2))
# print(addYears(datetime.date(2000,2,29),1))

# Expected Output :
# 2014-01-01
# 2015-01-01
# 2017-01-01
# 2001-03-01
import datetime
from datetime import date
def addYears(d, years):
    try:
#Return same day of the current year        
        return d.replace(year = d.year + years)
    except ValueError:
#If not same day, it will return other, i.e.  February 29 to March 1 etc.        
        return d + (date(d.year + years, 1, 1) - date(d.year, 1, 1))

print(addYears(datetime.date(2015,1,1), -1))
print(addYears(datetime.date(2015,1,1), 0))
print(addYears(datetime.date(2015,1,1), 2))
print(addYears(datetime.date(2000,2,29),1))


# 17. drop microseconds from datetime.
import datetime
dt = datetime.datetime.now().replace(microsecond=0)
print()
print(dt)
print()



# 18. get days between two dates.
# Sample Dates : 2000,2,28, 2001,2,28
# Expected Output : 366 days, 0:00:00
from datetime import date
a = date(2000,2,28)
b = date(2001,2,28)
print(b-a)



# 19. get the date of the last Tuesday.
from datetime import date
from datetime import timedelta
today = date.today()
offset = (today.weekday() - 1) % 7
last_tuesday = today - timedelta(days=offset)
print(last_tuesday)



# 20. test the third Tuesday of a month.

from datetime import datetime 
def is_third_tuesday(s):
    d = datetime.strptime(s, '%b %d, %Y')
    return d.weekday() == 1 and 14 < d.day < 22

print(is_third_tuesday('Jun 23, 2015')) #False
print(is_third_tuesday('Jun 16, 2015')) #True 
print(is_third_tuesday('Jul 21, 2015')) #False


# 21. get the last day of a specified year and month.

import calendar
year = 2015
month = 2
print(calendar.monthrange(year, month)[1])


# 22. get the number of days of a given month and year.
from calendar import monthrange
year = 2016
month = 2
print(monthrange(year, month))



# 23. add a month with a specified date.

from datetime import date, timedelta
import calendar
start_date = date(2014, 12, 25)
days_in_month = calendar.monthrange(start_date.year, start_date.month)[1]
print(start_date + timedelta(days=days_in_month))


# 24. count the number of Monday of the 1st day of the month from 2015 to 2016.

import datetime
from datetime import datetime
monday1 = 0
months = range(1,13)
for year in range(2015, 2017):
    for month in months:
        if datetime(year, month, 1).weekday() == 0:
            monday1 += 1
print(monday1)


# 25. print a string five times, delay three seconds.

import time
print("\nw3resource will print five  times, delay for three seconds.")
for _ in range(5):
    print("w3resource")
    time.sleep(3)
# 26. Write a Python program calculates the date six months from the current date using the datetime module.


import datetime
print((datetime.date.today() + datetime.timedelta(6*365/12)).isoformat())

# 27. create 12 fixed dates from a specified date over a given period. The difference between two dates will be 20.

import datetime
def every_20_days(date):
    print('Starting Date: {d}'.format(d=date))
    print("Next 12 days :")
    for _ in range(12):
        date=date+datetime.timedelta(days=20)
        print('{d}'.format(d=date))

dt = datetime.date(2016,8,1)
every_20_days(dt)


# 28. get the dates 30 days before and after from the current date.

from datetime import date, timedelta

current_date = date.today().isoformat()   
days_before = (date.today()-timedelta(days=30)).isoformat()
days_after = (date.today()+timedelta(days=30)).isoformat()  

print("\nCurrent Date: ",current_date)
print("30 days before current date: ",days_before)
print("30 days after current date : ",days_after)


# 29. get the GMT and local current time.

from time import gmtime, strftime
import time
print("\nGMT: "+time.strftime("%a, %d %b %Y %I:%M:%S %p %Z", time.gmtime()))
print("Local: "+strftime("%a, %d %b %Y %I:%M:%S %p %Z\n"))


# 30. convert a date to the timestamp.


import time
import datetime
now = datetime.datetime.now()
print()
print(time.mktime(now.timetuple()))
print()

# 31. convert a string date to the timestamp.
import time
import datetime
s = "01/10/2016"
print()
print(time.mktime(datetime.datetime.strptime(s, "%d/%m/%Y").timetuple()))
print()

# 32. calculate a number of days between two dates.


import datetime
from datetime import date
def differ_days(date1, date2):

    a = date1
    b = date2
    return (a-b).days
print()
print(differ_days((date(2016,10,12)), date(2015,12,10)))
print(differ_days((date(2016,3,23)), date(2017,12,10)))
print()

# 33. calculate no of days between two datetimes.

import datetime
from datetime import datetime

def differ_days(date1, date2):
    a = date1
    b = date2
    return (a-b).days
print()
print(differ_days((datetime(2016,10,12,0,0,0)), datetime(2015,12,10,0,0,0)))
print(differ_days((datetime(2016,10,12,0,0,0)), datetime(2015,12,10,23,59,59)))
print()


# 34. display the date and time in a human-friendly string.

import time
print()
print(time.ctime())
print()


# 35. convert a date to Unix timestamp.
import datetime
import time
dt = datetime.datetime(2016, 2, 25, 23, 23)
print()
print("Unix Timestamp: ",(time.mktime(dt.timetuple())))
print()



# 36. calculate two date difference in seconds.
from datetime import datetime, time
def date_diff_in_Seconds(dt2, dt1):
  timedelta = dt2 - dt1
  return timedelta.days * 24 * 3600 + timedelta.seconds
#Specified date
date1 = datetime.strptime('2015-01-01 01:00:00', '%Y-%m-%d %H:%M:%S')
#Current date
date2 = datetime.now()
print("\n%d seconds" %(date_diff_in_Seconds(date2, date1)))
print()



# 37. convert two date difference in days, hours, minutes, seconds.
from datetime import datetime, time

def date_diff_in_seconds(dt2, dt1):
  timedelta = dt2 - dt1
  return timedelta.days * 24 * 3600 + timedelta.seconds

def dhms_from_seconds(seconds):
	minutes, seconds = divmod(seconds, 60)
	hours, minutes = divmod(minutes, 60)
	days, hours = divmod(hours, 24)
	return (days, hours, minutes, seconds)

#Specified date
date1 = datetime.strptime('2015-01-01 01:00:00', '%Y-%m-%d %H:%M:%S')

#Current date
date2 = datetime.now()

print("\n%d days, %d hours, %d minutes, %d seconds" % dhms_from_seconds(date_diff_in_seconds(date2, date1)))
print()



# 38. get last modified information of a file.
import os, time
def last_modified_fileinfo(filepath):
	
    filestat = os.stat(filepath)
    date = time.localtime((filestat.st_mtime))

    # Extract year, month and day from the date
    year = date[0]
    month = date[1]
    day = date[2]
    # Extract hour, minute, second
    hour = date[3]
    minute = date[4]
    second = date[5]

    	# Year
    strYear = str(year)[:]

    	# Month
    strMonth = f'0{str(month)}' if (month <=9) else str(month)
    	# Date
    strDay = f'0{str(day)}' if (day <=9) else str(day)
    return f"{strYear}-{strMonth}-{strDay} {str(hour)}:{str(minute)}:{str(second)}"
print()
print(last_modified_fileinfo('test.txt'))
print()



# 39. calculate an age in year.

from datetime import date

def calculate_age(dtob):
    today = date.today()
    return today.year - dtob.year - ((today.month, today.day) < (dtob.month, dtob.day))
print()
print(calculate_age(date(2006,10,12)))
print(calculate_age(date(1989,1,12)))
print()


# 40. get the current date time information.

import time
import datetime

print()
print(f"Time in seconds since the epoch: {time.time()}")
print("Current date and time: " , datetime.datetime.now())
print("Alternate date and time: " ,datetime.datetime.now().strftime("%y-%m-%d-%H-%M"))
print("Current year: ", datetime.date.today().strftime("%Y"))
print("Month of year: ", datetime.date.today().strftime("%B"))
print("Week number of the year: ", datetime.date.today().strftime("%W"))
print("Weekday of the week: ", datetime.date.today().strftime("%w"))
print("Day of year: ", datetime.date.today().strftime("%j"))
print("Day of the month : ", datetime.date.today().strftime("%d"))
print("Day of week: ", datetime.date.today().strftime("%A"))
print()


# 41.  generate a date and time as a string.

import datetime
# Current time
now = datetime.datetime.now()
# Make a note of the date and time in a string
# Date and time in string : 2016-11-05 11:24:24 PM
datestr = "# In string: " + now.strftime("%Y-%m-%d %H:%M:%S %p") + "\n"
print()
print(datestr)
print()


# 42. display formatted text output of a month and start weeks on Sunday.

import calendar
cal = calendar.TextCalendar(calendar.SUNDAY)
print('First Month - 2022')
print(cal.prmonth(2022, 1))


# 43. print a 3-column calendar for an entire year.

import calendar
cal = calendar.TextCalendar(calendar.SUNDAY)
# year: 2022, column width: 2, lines per week: 1 
# number of spaces between month columns: 1
# 3: no. of months per column.
print(cal.formatyear(2022, 2, 1, 1, 3))


# 44. display a calendar for a locale.

import calendar
cal = calendar.LocaleTextCalendar(locale='en_AU.utf8')
print(cal.prmonth(2025, 9))


# 45. get the current week.
import datetime
Jan1st = datetime.date(2017,10,12)
year,week_num,day_of_week = Jan1st.isocalendar() # DOW = day of week
print()
print("Year %d, Week Number %d, Day of the Week %d" %(year,week_num, day_of_week))
print()



# 46. create a HTML calendar with data for a specific year and month.
import calendar
htmlcal = calendar.HTMLCalendar(calendar.MONDAY)
print(htmlcal.formatmonth(2020, 12))



# 47. Write a Python program display a list of the dates for the 2nd Saturday of every month for a given year.
import calendar
# Show every month
for month in range(1, 13):
    cal = calendar.monthcalendar(2020, month)
    first_week  = cal[0]
    second_week = cal[1]
    third_week  = cal[2]

    # If a Saturday presents in the first week, the second Saturday
    # is in the second week.  Otherwise, the second Saturday must 
    # be in the third week.

    if first_week[calendar.SATURDAY]:
        holi_day = second_week[calendar.SATURDAY]
    else:
        holi_day = third_week[calendar.SATURDAY]

    print('%3s: %2s' % (calendar.month_abbr[month], holi_day))



# 48. display a simple, formatted calendar of a given year and month.
import calendar
print('Print a calendar for a year and month:')
month = int(input('Month (mm): '))
year = int(input('Year (yyyy): '))
print('\n')

calendar.setfirstweekday(calendar.SUNDAY)
cal = calendar.monthcalendar(year, month)

if len(str(month)) == 1:
    month = f'0{month}'

# Header
print(f'|++++++ {month}-{year} +++++|')
print('|Su Mo Tu We Th Fr Sa|')
print('|--------------------|')

# display calendar
border = '|'
for week in cal:
    line = border


    for day in week:
        if day == 0:
      # 3 spaces for blank days       
         line += '   ' 
        elif len(str(day)) == 1:
            line += ' %d ' % day
        else:
         line += '%d ' % day
    # remove space in last column
    line = line[:-1]
    line += border
    print(line)

print('|--------------------|\n')



# 49. convert a string into datetime

from datetime import datetime

date_obj = datetime.strptime('May 12 2016  2:25AM', '%b %d %Y %I:%M%p')
print()
print(date_obj)
print()


# 50. get a list of dates between two dates.

from datetime import timedelta, date

def daterange(date1, date2):
    for n in range(int ((date2 - date1).days)+1):
        yield date1 + timedelta(n)

start_dt = date(2015, 12, 20)
end_dt = date(2016, 1, 11)
for dt in daterange(start_dt, end_dt):
    print(dt.strftime("%Y-%m-%d"))
	

# 51. generate RFC 3339 timestamp.

from datetime import datetime, timezone
local_time = datetime.now(timezone.utc).astimezone()
print()
print(local_time.isoformat())
print()


# 52. get the first and last second.

import datetime
print("First Second: ", datetime.time.min)
print("Last Second: ", datetime.time.max)


# 53. validate a Gregorian date. The month is between 1 and 12 inclusive, the day is within the allowed number of days for the given month. Leap year's are taken into consideration. The year is between 1 and 32767 inclusive.

import datetime
def check_date(m, d, y):
    try:
        m, d, y = map(int, (m, d, y))
        datetime.date(y, m, d)
        return True
    except ValueError:
        return False

print(check_date(11, 11, 2002))
print(check_date('11', '11', '2002'))
print(check_date(13, 11, 2002))


# 54. set the default timezone used by all date/time functions.
import os, time
print(time.strftime('%Y-%m-%d %H:%M:%S')) # before timezone change
os.environ['TZ'] = 'America/Los_Angeles' # set new timezone
time.tzset()
print(time.strftime('%Y-%m-%d %H:%M:%S')) # after timezone change



# 55. The epoch is the point where the time starts, and is platform dependent. For Unix, the epoch is January 1, 1970, 00:00:00 (UTC). find out what the epoch is on a given platform. Also convert a given time in seconds since the epoch.
# Sample Output:
# Epoch on a given platform:
# time.struct_time(tm_year=1970, tm_mon=1, tm_mday=1, tm_hour=0, tm_min=0, tm_sec=0, tm_wday=3, tm_yday=1, tm_isdst=0)
# Time in seconds since the epoch:
# time.struct_time(tm_year=1970, tm_mon=1, tm_mday=1, tm_hour=10, tm_min=0, tm_sec=0, tm_wday=3, tm_yday=1, tm_isdst=0)


import time
print("\nEpoch on a given platform:")
print(time.gmtime(0))
print("\nTime in seconds since the epoch:")
print(time.gmtime(36000))

# 56. get time values with components using local time and gmtime.
# Sample Output:
# localtime:
# tm_year : 2021
# tm_mon : 4
# tm_mday : 13
# tm_hour : 11
# tm_min : 20
# tm_sec : 37
# tm_wday : 1
# tm_yday : 103
# tm_isdst: 0
# gmtime:
# tm_year : 2021
# tm_mon : 4
# tm_mday : 13
# tm_hour : 11
# tm_min : 20
# tm_sec : 37
# tm_wday : 1
# tm_yday : 103
# tm_isdst: 0
import time
def time_struct(s):
   print(' tm_year :', s.tm_year)
   print(' tm_mon :', s.tm_mon)
   print(' tm_mday :', s.tm_mday)
   print(' tm_hour :', s.tm_hour)
   print(' tm_min :', s.tm_min)
   print(' tm_sec :', s.tm_sec)
   print(' tm_wday :', s.tm_wday)
   print(' tm_yday :', s.tm_yday)
   print(' tm_isdst:', s.tm_isdst)
print('\nlocaltime:')
time_struct(time.localtime())
print('\ngmtime:')
time_struct(time.gmtime())



# 57. get different time values with components timezone, timezone abbreviations, the offset of the local (non-DST) timezone, DST timezone and time of different timezones.
# Sample Output:
# Default Zone:
# TZ : (not set)
# Timezone abbreviations: ('UTC', 'UTC')
# Timezone : 0 (0.0)
# DST timezone 0
# Time : 11:30:05 04/13/21 UTC
# Pacific/Auckland :
# TZ : Pacific/Auckland
# Timezone abbreviations: ('NZST', 'NZDT')
# Timezone : -43200 (-12.0)
# DST timezone 1
# Time : 23:30:05 04/13/21 NZST
# Europe/Berlin :
# TZ : Europe/Berlin
# Timezone abbreviations: ('CET', 'CEST')
# Timezone : -3600 (-1.0)
# DST timezone 1
# Time : 13:30:05 04/13/21 CEST
# America/Detroit :
# TZ : America/Detroit
# Timezone abbreviations: ('EST', 'EDT')
# Timezone : 18000 (5.0)
# DST timezone 1
# Time : 07:30:05 04/13/21 EDT
# Singapore :
# TZ : Singapore
# Timezone abbreviations: ('+08', '+08')
# Timezone : -28800 (-8.0)
# DST timezone 0
# Time : 19:30:05 04/13/21 +08
import time
import os
def zone_info():
    print('TZ   :', os.environ.get('TZ', '(not set)'))
    print('Timezone abbreviations:', time.tzname)
    print(f'Timezone : {time.timezone} ({time.timezone / 3600})')
    print('DST timezone ', time.daylight)
    print('Time :', time.strftime('%X %x %Z'),'\n')
print('Default Zone:')
zone_info()
TIME_ZONES = [
   'Pacific/Auckland',
   'Europe/Berlin',
   'America/Detroit',
   'Singapore',
]
for zone in TIME_ZONES:
   os.environ['TZ'] = zone
   time.tzset()
   print(zone, ':')
   zone_info()



# 58.can suspend execution of a given script a given number of seconds.
# Sample Output:
# Sorry, Slept for 3 seconds...
# Sorry, Slept for 3 seconds...
# Sorry, Slept for 3 seconds...
# Sorry, Slept for 3 seconds...

import time
for _ in range(4):
    time.sleep(3)
    print("Sorry, Slept for 3 seconds...")


# 59. convert a given time in seconds since the epoch to a string representing local time.
# Sample Output:
# Tue Apr 13 11:51:51 2021
# Thu Jun 30 18:36:29 1977

import time
print(time.ctime())
print(time.ctime(236543789))


# 60. print simple format of time, full names and the representation format and preferred date time format.
# Sample Output:
# Simple format of time:
# Tue, 13 Apr 2021 12:02:01 + 1010
# Full names and the representation:
# Tuesday, 04/13/21 April 2021 12:02:01 + 0000
# Preferred date time format:
# Tue Apr 13 12:02:01 2021
# Example 11: 04/13/21, 12:02:01, 21, 2021
import time
print("\nSimple format of time:")
s = time.strftime("%a, %d %b %Y %H:%M:%S + 1010", time.gmtime())
print(s)
print("\nFull names and the representation:")
s = time.strftime("%A, %D %B %Y %H:%M:%S + 0000", time.gmtime())
print( s)
print("\nPreferred date time format:")
s = time.strftime("%c")
print(s)
s = time.strftime("%x, %X, %y, %Y")
print("Example 11:", s)



# 61.takes a given number of seconds and pass since epoch as an argument. Print structure time in local time.
# Sample Output:
# Result: time.struct_time(tm_year=1983, tm_mon=2, tm_mday=19, tm_hour=21, tm_min=38, tm_sec=18, tm_wday=5, tm_yday=50, tm_isdst=0)
# Year: 1983

import time
result = time.localtime(414538698)
print("\nResult:", result)
print("\nYear:", result.tm_year)


# 62.takes a tuple containing 9 elements corresponding to structure time as an argument and returns a string representing it.
# Sample Output:
# Result: Sun Jan 22 02:34:06 2020
# Result: Tue Nov 12 02:54:08 1982

import time
t = (2020, 1, 22, 2, 34, 6, 6, 362, 0)
result = time.asctime(t)
print("Result:", result)
t = (1982, 11, 12, 2, 54, 8, 8, 360, 0)
result = time.asctime(t)
print("Result:", result)


# 63. parse a string representing time and returns the structure time.
# Sample Output:
# String representing time: 22 January, 2020
# time.struct_time(tm_year=2020, tm_mon=1, tm_mday=22, tm_hour=0, tm_min=0, tm_sec=0, tm_wday=2, tm_yday=22, tm_isdst=-1)
# String representing time: 30 Nov 00
# time.struct_time(tm_year=2000, tm_mon=11, tm_mday=30, tm_hour=0, tm_min=0, tm_sec=0, tm_wday=3, tm_yday=335, tm_isdst=-1)
# String representing time: 04/11/15 11:55:23
# time.struct_time(tm_year=2015, tm_mon=4, tm_mday=11, tm_hour=11, tm_min=55, tm_sec=23, tm_wday=5, tm_yday=101, tm_isdst=-1)
# String representing time: 12-11-2019
# time.struct_time(tm_year=2019, tm_mon=12, tm_mday=11, tm_hour=0, tm_min=0, tm_sec=0, tm_wday=2, tm_yday=345, tm_isdst=-1)
# String representing time: 13::55::26
# time.struct_time(tm_year=1900, tm_mon=1, tm_mday=1, tm_hour=13, tm_min=55, tm_sec=26, tm_wday=0, tm_yday=1, tm_isdst=-1)import time
time_string = "22 January, 2020"
print("String representing time:",time_string)
result = time.strptime(time_string, "%d %B, %Y")
print(result)
time_string = "30 Nov 00"
print("\nString representing time:",time_string)
result = time.strptime(time_string, "%d %b %y")
print(result)
time_string = '04/11/15 11:55:23'
print("\nString representing time:",time_string)
result = time.strptime(time_string, "%m/%d/%y %H:%M:%S")
print(result)
time_string = '12-11-2019'
print("\nString representing time:",time_string)
result = time.strptime(time_string, "%m-%d-%Y")
print(result)
time_string = '13::55::26'
print("\nString representing time:",time_string)
result = time.strptime(time_string, "%H::%M::%S")
print(result)




Dicitonary.py """
1. Write a Python script to sort (ascending and descending) a dictionary
by value.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

print(sorted(my_dict.items(), key=lambda x: x[1]))
print(sorted(my_dict.items(), key=lambda x: x[1], reverse=True))
    
"""
2. Write a Python script to add a key to a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

my_dict['f'] = 6
print(my_dict)

"""
3. Write a Python script to concatenate following dictionaries to create
a new one. 
Sample Dictionary : 
dic1={1:10, 2:20} 
dic2={3:30, 4:40} 
dic3={5:50, 6:60}
Expected Result : {1: 10, 2: 20, 3: 30, 4: 40, 5: 50, 6: 60}
"""

dict1={1:10, 2:20} 
dict2={3:30, 4:40} 
dict3={5:50, 6:60}

combined_dict = {k: v for k, v in dict1.items() | dict2.items() | dict3.items()}
print(combined_dict)

"""
4. Write a Python script to check if a given key already exists in a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

print('a' in my_dict)
print('x' in my_dict)

"""
5. Write a Python program to iterate over a dictionary using for loop.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

for k, v in my_dict.items():
    print(k, v)

"""
6.Write a Python script to generate and print a dictionary that contains
a number (between 1 and n) in the form (x, x*x).
Sample: (n = 5) 
Expected Output: {1: 1, 2: 4, 3: 9, 4: 16, 5: 25}
"""

n = 5

my_dict = {x: x**2 for x in range(1, n+1)}
print(my_dict)

"""
7. Write a Python script to print a dictionary where the keys are numbers
between 1 and 15 (both included) and the values are square of keys.
Sample Dictionary 
{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100, 11: 121,
12: 144, 13: 169, 14: 196, 15: 225}
"""

my_dict = {x: x**2 for x in range(1, 16)}
print(my_dict)

"""
8. Write a Python script to merge two Python dictionaries.
"""

# solution 1
dict_one = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}
dict_two = {'f': 6, 'g': 7, 'h': 8, 'i': 9, 'j': 0}

for key in dict_two:                 # dict_one.update(dict_two)
    dict_one[key] = dict_two[key]  

print(dict_one)

# soltuion 2
dict_one = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}
dict_two = {'f': 6, 'g': 7, 'h': 8, 'i': 9, 'j': 0}

combined_dict = {}
combined_dict.update(dict_one, **dict_two)
print(combined_dict)

"""
10. Write a Python program to sum all the items in a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

# solution 1
dict_sum = sum(my_dict.values())
print(dict_sum)

# solution 2
dict_sum = 0
for k, v in my_dict.items():
    dict_sum += v
print(dict_sum)

"""
12. Write a Python program to remove a key from a dictionary.
"""

# solution 1
my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}
if my_dict['a']:
    del my_dict['a']
print(my_dict)

# solution 2
my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}
my_dict.pop('a', 0)
print(my_dict)

"""
13. Write a Python program to map two lists into a dictionary.
"""

list_one = ['a', 'b', 'c', 'd']
list_two = [1, 2, 3, 4]

# solution 1
my_dict = {k: v for (k, v) in zip(list_one, list_two)}
print(my_dict)

# solution 2
my_dict = dict(zip(list_one, list_two))
print(my_dict)

"""
14. Write a Python program to sort a dictionary by key.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

for key in sorted(my_dict.keys()):
    print(key, ":", my_dict[key])

"""
15. Write a Python program to get the maximum and minimum value in a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}

dict_min = min(my_dict.values())
dict_max = max(my_dict.values())

print('in dictionary', my_dict, 'max is %d and min is %d' % (dict_max, dict_min))

"""
16.Write a Python program to get a dictionary from an object's fields.
"""

print(dict.__dict__)

"""
17. Write a Python program to remove duplicates from Dictionary.
"""

# solution 1
my_dict = {'a': 1, 'b': 2, 'c': 1, 'd': 2, 'e': 5}

unique_values = []
keys_to_remove = []

for key in my_dict:
    if my_dict[key] in unique_values:
        keys_to_remove.append(key)
    else:
        unique_values.append(my_dict[key])

for key in keys_to_remove:
    del my_dict[key]        # my_dict.pop(key)

print(my_dict)

# solution 2
my_dict = {'a': 1, 'b': 2, 'c': 1, 'd': 2, 'e': 5}

new_dict = {}

for key, value in my_dict.items():
    if value not in new_dict.values():
        new_dict[key] = value

print(new_dict)

"""
18. Write a Python program to check a dictionary is empty or not.
"""

my_dict = {}
my_other_dict = {'a': 1}

if not my_dict:
    print("dicttionary", my_dict, "is empty")
else:
    print("dicttionary", my_dict, "is NOT empty")

if not my_other_dict:
    print("dicttionary", my_other_dict, "is empty")
else:
    print("dicttionary", my_other_dict, "is NOT empty")

"""
19. Write a Python program to combine two dictionary adding values
for common keys. 
d1 = {'a': 100, 'b': 200, 'c':400}
d2 = {'a': 300, 'b': 200, 'd':400}
Sample output: Counter({'a': 400, 'b': 400, 'd': 400, 'c': 300})
"""

d1 = {'a': 100, 'b': 200, 'c':400}
d2 = {'a': 300, 'b': 200, 'd':400}
counter = {}
counter = d1.copy()

for key in d2:
    if key in counter:
        counter[key] += d2[key]
    else:
        counter[key] = d2[key]
print(counter)

"""
20. Write a Python program to print all unique values in a dictionary.
Sample Data: [{"V": "S001"}, {"V": "S002"}, {"VI": "S001"},
            {"VI": "S005"}, {"VII": "S005"}, {"V": "S009"}, {"VIII": "S007"}]
Expected Output: Unique Values: {'S005', 'S002', 'S007', 'S001', 'S009'}
"""

my_list = [{"V": "S001"}, {"V": "S002"}, {"VI": "S001"},
            {"VI": "S005"}, {"VII": "S005"}, {"V": "S009"}, {"VIII": "S007"}]
# solution 1
my_set = set()
for dictionary in my_list:
    for k, v in dictionary.items():
        my_set.add(v)
print(my_set)

# solution 1 with set comprehension
my_set = {v for d in my_list for k, v in d.items()}
print(my_set)

"""
21. Write a Python program to create and display all combinations of letters,
selecting each letter from a different key in a dictionary. 
Sample data : {'1':['a','b'], '2':['c','d']}
Expected Output: 
ac
ad
bc
bd
"""

my_dict = {'1':['a','b'], '2':['c','d']}

my_list = list(my_dict.values())
print(my_list)
for i in my_list[0]:
    for j in range(1, len(my_list)):
        for x in my_list[j]:
            my_string = i + x
            print(my_string)

"""
22. Write a Python program to find the highest 3 values in a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5, 'f': 5, 'x': 2}
sorted_values_list = sorted(set(my_dict.values()))
print("the highest 3 values are", sorted_values_list[-3:])
keys_with_highest_values = []
for key in my_dict:
    if my_dict[key] in sorted_values_list[-3:]:
        keys_with_highest_values.append(key)
print("their keys are (not in order)", keys_with_highest_values)

"""
23. Write a Python program to combine values in python list of dictionaries.
Sample data: [{'item': 'item1', 'amount': 400}, {'item': 'item2', 'amount': 300},
{'item': 'item1', 'amount': 750}]
Expected Output: Counter({'item1': 1150, 'item2': 300})
"""

my_list = [{'item': 'item1', 'amount': 400},
           {'item': 'item2', 'amount': 300},
           {'item': 'item1', 'amount': 750}]

new_list = []
for d in my_list:
    new_list.append(tuple(d.values()))
combined_dict = {}
for t in new_list:
    if t[0] in combined_dict:
        combined_dict[t[0]] += t[1]
    else:
        combined_dict[t[0]] = t[1]
print(combined_dict)

"""
24. Write a Python program to create a dictionary from a string. 
Note: Track the count of the letters from the string.
Sample string : 'w3resource'
Expected output: {'3': 1, 's': 1, 'r': 2, 'u': 1, 'w': 1, 'c': 1, 'e': 2, 'o': 1}
"""

my_string = 'w3resource'

my_dict = {letter: my_string.count(letter) for letter in my_string}
print(my_dict)

"""
25. Write a Python program to print a dictionary in table format.
"""

my_dict = {'alice': 11, 'benji': 24, 'cilian': 38, 'david': 42, 'ewan': 56}

for key, value in my_dict.items():
    print('{:<7s}'.format(key), '{:>3d}'.format(value))

"""
26. Write a Python program to count the values associated with key in a dictionary.
Sample data: = [{'id': 1, 'success': True, 'name': 'Lary'}, {'id': 2, 'success': False, 'name': 'Rabi'}, {'id': 3, 'success': True, 'name': 'Alex'}]
Expected result: Count of how many dictionaries have success as True
"""

my_list = [{'id': 1, 'success': True, 'name': 'Lary'},
           {'id': 2, 'success': False, 'name': 'Rabi'},
           {'id': 3, 'success': True, 'name': 'Alex'}]

# solution 1
success_count = 0
for dictionary in my_list:
    if 'success' in dictionary:
        if dictionary['success'] == True:
            success_count += 1
print(success_count)
    
"""
27. Write a Python program to convert a list into a nested dictionary of keys.
"""

my_list = [1, 2, 3, 4, 5]
my_dict = {}

"""
28. Write a Python program to sort a list alphabetically in a dictionary.
"""

my_dict = {'a': [2, 3, 5],
           'b': [1, 8, 4],
           'c': [9, 0, 1]}

for li in my_dict.values():
    li.sort()
print(my_dict)

"""
29. Write a Python program to remove spaces from dictionary keys.
"""

my_dict = {'a b c': [2, 3, 5],
           'b d e': [1, 8, 4],
           'c f g': [9, 0, 1]}

for key in my_dict:
    new_key = key.replace(' ', '')
    my_dict[new_key] = my_dict.pop(key)
print(my_dict)

"""
30. Write a Python program to get the top three items in a shop.
Sample data: {'item1': 45.50, 'item2':35, 'item3': 41.30,
'item4':55, 'item5': 24}
Expected Output: 
item4 55
item1 45.5
item3 41.3
"""

items_dict = {'item1': 45.50, 'item2':35, 'item3': 41.30, 'item4':55, 'item5': 24}

my_list = [(k, v) for k,v in items_dict.items()]
my_list.sort(key=lambda x: x[1], reverse=True)
for i in my_list[:3]:
    print('{:<5s}'.format(i[0]), '{:<5.2f}'.format(i[1]))

"""
31. Write a Python program to get the key, value and item in a dictionary.
"""

my_dict = {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5, 'f': 5, 'x': 2}

for k, v in my_dict.items():
    print('key:', k, 'value:', v, 'item:', (k, v))

"""
32. Write a Python program to print a dictionary line by line.
"""

my_dict = {'alice': 11, 'benji': 24, 'cilian': 38, 'david': 42, 'ewan': 56}

for k,v in my_dict.items():
    print(k)
    print(v)

"""
33. Write a Python program to check multiple keys exists in a dictionary.
"""

my_dict = {'a': 1}
my_other_dict = {'a': 1, 'b': 2}
my_empty_dict = {}

if len(my_dict.keys()) > 1:
    print('dictionary', my_dict, 'has multiple keys')
else:
    print('dictionary', my_dict, 'does not have multiple keys')

if len(my_other_dict.keys()) > 1:
    print('dictionary', my_other_dict, 'has multiple keys')
else:
    print('dictionary', my_other_dict, 'does not have multiple keys')

if len(my_empty_dict.keys()) > 1:
    print('dictionary', my_empty_dict, 'has multiple keys')
else:
    print('dictionary', my_empty_dict, 'does not have multiple keys')

"""
34. Write a Python program to count number of items in a dictionary value
that is a list.
"""

my_dict = {'a': [2, 3, 5],
           'b': [1, 8, 4],
           'c': [9, 0, 1],
           'd': 3}

for k, v in my_dict.items():
    if isinstance(v, list):
        print('key=', k, 'value lenth=', len(v))
    else:
        print('key=', k, 'value is not a list')

"""
35. Write a Python program to sort Counter by value. 
Sample data : {'Math':81, 'Physics':83, 'Chemistry':87}
Expected data: [('Chemistry', 87), ('Physics', 83), ('Math', 81)]
"""

my_dict = {'Math':81, 'Physics':83, 'Chemistry':87}
my_list = list(my_dict.items())
my_list.sort(key=lambda x: x[1], reverse= True)
print(my_list)

"""
36. Write a Python program to create a dictionary from two lists
without losing duplicate values. 
Sample lists: ['Class-V', 'Class-VI', 'Class-VII', 'Class-VIII'], [1, 2, 2, 3]
Expected Output: defaultdict(<class 'set'>, {'Class-VII': {2}, 'Class-VI': {2},
'Class-VIII': {3}, 'Class-V': {1}})
"""

list_one = ['Class-V', 'Class-VI', 'Class-VII', 'Class-VIII']
list_two = [1, 2, 2, 3]

my_dict = {k: v for (k, v) in zip(list_one, list_two)}
print(my_dict)

"""
37. Write a Python program to replace dictionary values with their sum.
"""

my_dict = {'a': [2, 3, 5],
           'b': [1, 8, 4],
           'c': [9, 0, 1]}

my_new_dict = {k: sum(v) for k, v in my_dict.items()}
print(my_new_dict)

"""
38. Write a Python program to match key values in two dictionaries. 
Sample dictionary: {'key1': 1, 'key2': 3, 'key3': 2}, {'key1': 1, 'key2': 2}
Expected output: key1: 1 is present in both x and y
"""
dict_one = {'key1': 1, 'key2': 3, 'key3': 2}
dict_two = {'key1': 1, 'key2': 2}
l1=sorted(list(dict_one))
l2=sorted(list(dict_two))
l_intersection=l1 and l2
for key in l_intersection:
    print("Key is present in both", l_intersection)
    break

# 39. Write a Python program to store a given dictionary in a json file. 
# Original dictionary:
# {'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}
# <class 'dict'>
# Json file to dictionary:
# {'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}

d = {"students":[{"firstName": "Nikki", "lastName": "Roysden"},
               {"firstName": "Mervin", "lastName": "Friedland"},
               {"firstName": "Aron ", "lastName": "Wilkins"}],
"teachers":[{"firstName": "Amberly", "lastName": "Calico"},
         {"firstName": "Regine", "lastName": "Agtarap"}]}
print("Original dictionary:")
print(d)
print(type(d))
import json
 
with open("dictionary", "w") as f:
   json.dump(d, f, indent = 4, sort_keys = True)
 
print("\nJson file to dictionary:")
with open('dictionary') as f:
 data = json.load(f)
print(data)

# 40. Write a Python program to create a dictionary of keys x, y, and z where each key has as value a list from 11-20, 21-30, and 31-40 respectively. Access the fifth value of each key from the dictionary. 
# {'x': [11, 12, 13, 14, 15, 16, 17, 18, 19],
# 'y': [21, 22, 23, 24, 25, 26, 27, 28, 29],
# 'z': [31, 32, 33, 34, 35, 36, 37, 38, 39]}
# 15
# 25
# 35
# x has value [11, 12, 13, 14, 15, 16, 17, 18, 19]
# y has value [21, 22, 23, 24, 25, 26, 27, 28, 29]
# z has value [31, 32, 33, 34, 35, 36, 37, 38, 39]
d = {"students":[{"firstName": "Nikki", "lastName": "Roysden"},
               {"firstName": "Mervin", "lastName": "Friedland"},
               {"firstName": "Aron ", "lastName": "Wilkins"}],
"teachers":[{"firstName": "Amberly", "lastName": "Calico"},
         {"firstName": "Regine", "lastName": "Agtarap"}]}
print("Original dictionary:")
print(d)
print(type(d))
import json
 
with open("dictionary", "w") as f:
   json.dump(d, f, indent = 4, sort_keys = True)
 
print("\nJson file to dictionary:")
with open('dictionary') as f:
 data = json.load(f)
print(data)


# 41. Write a Python program to drop empty Items from a given Dictionary. 
# Original Dictionary:
# {'c1': 'Red', 'c2': 'Green', 'c3': None}
# New Dictionary after dropping empty items:
# {'c1': 'Red', 'c2': 'Green'}

dict1 = {'c1': 'Red', 'c2': 'Green', 'c3':None}
print("Original Dictionary:")
print(dict1)
print("New Dictionary after dropping empty items:")
dict1 = {key:value for (key, value) in dict1.items() if value is not None}
print(dict1)

# 42. Write a Python program to filter a dictionary based on values. 
# Original Dictionary:
# {'Cierra Vega': 175, 'Alden Cantrell': 180, 'Kierra Gentry': 165, 'Pierre Cox': 190}
# Marks greater than 170:
# {'Cierra Vega': 175, 'Alden Cantrell': 180, 'Pierre Cox': 190}

marks = {'Cierra Vega': 175, 'Alden Cantrell': 180, 'Kierra Gentry': 165, 'Pierre Cox': 190}
print("Original Dictionary:")
print(marks)
print("Marks greater than 170:")
result = {key:value for (key, value) in marks.items() if value >= 170}
print(result)

# 43. Write a Python program to convert more than one list to nested dictionary. 
# Original strings:
# ['S001', 'S002', 'S003', 'S004']
# ['Adina Park', 'Leyton Marsh', 'Duncan Boyle', 'Saim Richards']
# [85, 98, 89, 92]
# Nested dictionary:
# [{'S001': {'Adina Park': 85}}, {'S002': {'Leyton Marsh': 98}}, {'S003': {'Duncan Boyle': 89}}, {'S004': {'Saim Richards': 92}}]

def nested_dictionary(l1, l2, l3):
     result = [{x: {y: z}} for (x, y, z) in zip(l1, l2, l3)]
     return result

student_id = ["S001", "S002", "S003", "S004"] 
student_name = ["Adina Park", "Leyton Marsh", "Duncan Boyle", "Saim Richards"] 
student_grade = [85, 98, 89, 92]
print("Original strings:")
print(student_id)
print(student_name)
print(student_grade)
print("\nNested dictionary:")
ch='a'
print(nested_dictionary(student_id, student_name, student_grade))

# 44. Write a Python program to filter the height and width of students, which are stored in a dictionary. 
# Original Dictionary:
# {'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}
# Height > 6ft and Weight> 70kg:
# {'Cierra Vega': (6.2, 70)}

def filter_data(students):
    result = {k: s for k, s in students.items() if s[0] >=6.0 and s[1] >=70}
    return result    
 
students = {'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}
print("Original Dictionary:")
print(students)
print("\nHeight > 6ft and Weight> 70kg:")
print(filter_data(students))

# 45. Write a Python program to check all values are same in a dictionary. 
# Original Dictionary:
# {'Cierra Vega': 12, 'Alden Cantrell': 12, 'Kierra Gentry': 12, 'Pierre Cox': 12}
# Check all are 12 in the dictionary.
# True
# Check all are 10 in the dictionary.
# False

def value_check(students, n):
    result = all(x == n for x in students.values()) 
    return result
  
students = {'Cierra Vega': 12, 'Alden Cantrell': 12, 'Kierra Gentry': 12, 'Pierre Cox': 12}
print("Original Dictionary:")
print(students)
n = 12
print("\nCheck all are ",n,"in the dictionary.")
print(value_check(students, n))
n = 10
print("\nCheck all are ",n,"in the dictionary.")
print(value_check(students, n))

# 46. Write a Python program to create a dictionary grouping a sequence of key-value pairs into a dictionary of lists. 
# Original list:
# [('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]
# Grouping a sequence of key-value pairs into a dictionary of lists:
# {'yellow': [1, 3], 'blue': [2, 4], 'red': [1]}
def grouping_dictionary(l):
    result = {}
    for k, v in l:
         result.setdefault(k, []).append(v)
    return result
colors = [('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]
print("Original list:")
print(colors)
print("\nGrouping a sequence of key-value pairs into a dictionary of lists:")
print(grouping_dictionary(colors))


# 47. Write a Python program to split a given dictionary of lists into list of dictionaries. 
# Original dictionary of lists:
# {'Science': [88, 89, 62, 95], 'Language': [77, 78, 84, 80]}
# Split said dictionary of lists into list of dictionaries:
# [{'Science': 88, 'Language': 77}, {'Science': 89, 'Language': 78}, {'Science': 62, 'Language': 84}, {'Science': 95, 'Language': 80}]
def list_of_dicts(marks):
    keys = marks.keys()
    vals = zip(*[marks[k] for k in keys])
    result = [dict(zip(keys, v)) for v in vals]
    return result

marks = {'Science': [88, 89, 62, 95], 'Language': [77, 78, 84, 80]}
print("Original dictionary of lists:")
print(marks)
print("\nSplit said dictionary of lists into list of dictionaries:")
print(list_of_dicts(marks))


# 48. Write a Python program to remove a specified dictionary from a given list. 
# Original list of dictionary:
# [{'id': '#FF0000', 'color': 'Red'}, {'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]
# Remove id #FF0000 from the said list of dictionary:
# [{'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]

def remove_dictionary(colors, r_id):
    colors[:] = [d for d in colors if d.get('id') != r_id]
    return colors

colors = [{"id" : "#FF0000", "color" : "Red"}, 
          {"id" : "#800000", "color" : "Maroon"}, 
          {"id" : "#FFFF00", "color" : "Yellow"}, 
          {"id" : "#808000", "color" : "Olive"}] 
print('Original list of dictionary:')
print(colors)
r_id = "#FF0000"
print("\nRemove id",r_id,"from the said list of dictionary:")
print(remove_dictionary(colors, r_id))

# 49. Write a Python program to convert string values of a given dictionary, into integer/float datatypes. 
# Original list:
# [{'x': '10', 'y': '20', 'z': '30'}, {'p': '40', 'q': '50', 'r': '60'}]
# String values of a given dictionary, into integer types:
# [{'x': 10, 'y': 20, 'z': 30}, {'p': 40, 'q': 50, 'r': 60}]
# Original list:
# [{'x': '10.12', 'y': '20.23', 'z': '30'}, {'p': '40.00', 'q': '50.19', 'r': '60.99'}]
# String values of a given dictionary, into float types:
# [{'x': 10.12, 'y': 20.23, 'z': 30.0}, {'p': 40.0, 'q': 50.19, 'r': 60.99}]

def convert_to_int(lst):
    result = [dict([a, int(x)] for a, x in b.items()) for b in lst]
    return result

def convert_to_float(lst):
    result = [dict([a, float(x)] for a, x in b.items()) for b in lst]
    return result

nums =[{ 'x':'10' , 'y':'20' , 'z':'30' }, { 'p':'40', 'q':'50', 'r':'60'}]
print("Original list:")
print(nums)
print("\nString values of a given dictionary, into integer types:")
print(convert_to_int(nums))
nums =[{ 'x':'10.12', 'y':'20.23', 'z':'30'}, { 'p':'40.00', 'q':'50.19', 'r':'60.99'}]
print("\nOriginal list:")
print(nums)
print("\nString values of a given dictionary, into float types:")
print(convert_to_float(nums))

# 50. A Python Dictionary contains List as value. Write a Python program to clear the list values in the said dictionary. 
# Original Dictionary:
# {'C1': [10, 20, 30], 'C2': [20, 30, 40], 'C3': [12, 34]}
# Clear the list values in the said dictionary:
# {'C1': [], 'C2': [], 'C3': []}
def test(dictionary):
    for key in dictionary:
        dictionary[key].clear()
    return dictionary

dictionary = { 
               'C1' : [10,20,30], 
               'C2' : [20,30,40],
               'C3' : [12,34]
             }
print("\nOriginal Dictionary:")
print(dictionary)
print("\nClear the list values in the said dictionary:")
print(test(dictionary)) 


# 51. A Python Dictionary contains List as value. Write a Python program to update the list values in the said dictionary. 
# Original Dictionary:
# {'Math': [88, 89, 90], 'Physics': [92, 94, 89], 'Chemistry': [90, 87, 93]}
# Update the list values of the said dictionary:
# {'Math': [89, 90, 91], 'Physics': [90, 92, 87], 'Chemistry': [90, 87, 93]}
def test(dictionary):
    dictionary['Math'] = [x+1 for x in dictionary['Math']]
    dictionary['Physics'] = [x-2 for x in dictionary['Physics']]
    return dictionary

dictionary = { 
               'Math' : [88, 89, 90], 
               'Physics' : [92, 94, 89],
               'Chemistry' : [90, 87, 93]
             }
print("\nOriginal Dictionary:")
print(dictionary)
print("\nUpdate the list values of the said dictionary:")
print(test(dictionary))


# 52. Write a Python program to extract a list of values from a given list of dictionaries. 
# Original Dictionary:
# [{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]
# Extract a list of values from said list of dictionaries where subject = Science
# [92, 94, 88]
# Original Dictionary:
# [{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]
# Extract a list of values from said list of dictionaries where subject = Math
# [90, 89, 92]
def test(lst, marks):
    result = [d[marks] for d in lst if marks in d]
 
    return result

marks = [{'Math': 90, 'Science': 92}, 
         {'Math': 89, 'Science': 94}, 
         {'Math': 92, 'Science': 88}]

print("\nOriginal Dictionary:")
print(marks)
subj = "Science"
print("\nExtract a list of values from said list of dictionaries where subject =",subj)
print(test(marks, subj))

print("\nOriginal Dictionary:")
print(marks)
subj = "Math"
print("\nExtract a list of values from said list of dictionaries where subject =",subj)
print(test(marks, subj))


# 53. Write a Python program to find the length of a given dictionary values. 
# Original Dictionary:
# {1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}
# Length of dictionary values:
# {'red': 3, 'green': 5, 'black': 5, 'white': 5}
# Original Dictionary:
# {'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}
# Length of dictionary values:
# {'Austin Little': 13, 'Natasha Howard': 14, 'Alfred Mullins': 14, 'Jamie Rowe': 10}

def test(dictt):
    result = {}
    for val in dictt.values(): 
        result[val] = len(val) 
    return result    

color_dict = {1 : 'red', 2 : 'green', 3 : 'black', 4 : 'white', 5 : 'black'}
print("\nOriginal Dictionary:")
print(color_dict)
print("Length of dictionary values:")
print(test(color_dict))

color_dict = {'1' : 'Austin Little', '2' : 'Natasha Howard', '3' : 'Alfred Mullins', '4' : 'Jamie Rowe'}
print("\nOriginal Dictionary:")
print(color_dict)
print("Length of dictionary values:")
print(test(color_dict))

# 54. Write a Python program to get the depth of a dictionary. 
# Expected Output:
# 4
def dict_depth(d):
    if isinstance(d, dict):
        return 1 + (max(map(dict_depth, d.values())) if d else 0)
    return 0
dic = {'a':1, 'b': {'c': {'d': {}}}}
print(dict_depth(dic))


# 55. Write a Python program to access dictionary key's element by index. 
# Expected Output:
# physics
# math
# chemistry
num = {'physics': 80, 'math': 90, 'chemistry': 86}
print(list(num)[0])
print(list(num)[1])
print(list(num)[2])


# 56. Write a Python program to convert a given dictionary into a list of lists. 
# Original Dictionary:
# {1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}
# Convert the said dictionary into a list of lists:
# [[1, 'red'], [2, 'green'], [3, 'black'], [4, 'white'], [5, 'black']]
# Original Dictionary:
# {'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}
# Convert the said dictionary into a list of lists:
# [['1', 'Austin Little'], ['2', 'Natasha Howard'], ['3', 'Alfred Mullins'], ['4', 'Jamie Rowe']]
def test(dictt):
    result = list(map(list, dictt.items()))
    return result    

color_dict = {1 : 'red', 2 : 'green', 3 : 'black', 4 : 'white', 5 : 'black'}
print("\nOriginal Dictionary:")
print(color_dict)
print("Convert the said dictionary into a list of lists:")
print(test(color_dict))

color_dict = {'1' : 'Austin Little', '2' : 'Natasha Howard', '3' : 'Alfred Mullins', '4' : 'Jamie Rowe'}
print("\nOriginal Dictionary:")
print(color_dict)
print("Convert the said dictionary into a list of lists:")
print(test(color_dict))


# 57. Write a Python program to filter even numbers from a given dictionary values. 
# Original Dictionary:
# {'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}
# Filter even numbers from said dictionary values:
# {'V': [4, 6, 10], 'VI': [4, 12], 'VII': [8]}
# Original Dictionary:
# {'V': [1, 3, 5], 'VI': [1, 5], 'VII': [2, 7, 9]}
# Filter even numbers from said dictionary values:
# {'V': [], 'VI': [], 'VII': [2]}
def test(dictt):
    result = {key: [idx for idx in val if not idx % 2]  
          for key, val in dictt.items()}   
    return result    

students = {'V' : [1, 4, 6, 10], 'VI' : [1, 4, 12], 'VII' : [1, 3, 8]} 
print("\nOriginal Dictionary:")
print(students)
print("Filter even numbers from said dictionary values:")
print(test(students))

students = {'V' : [1, 3, 5], 'VI' : [1, 5], 'VII' : [2, 7, 9]} 
print("\nOriginal Dictionary:")
print(students)
print("Filter even numbers from said dictionary values:")
print(test(students))


# 58. Write a Python program to get all combinations of key-value pairs in a given dictionary. 
# Original Dictionary:
# {'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}
# Combinations of key-value pairs of the said dictionary:
# [{'V': [1, 4, 6, 10], 'VI': [1, 4, 12]}, {'V': [1, 4, 6, 10], 'VII': [1, 3, 8]}, {'VI': [1, 4, 12], 'VII': [1, 3, 8]}]
# Original Dictionary:
# {'V': [1, 3, 5], 'VI': [1, 5]}
# Combinations of key-value pairs of the said dictionary:
# [{'V': [1, 3, 5], 'VI': [1, 5]}]
import itertools
def test(dictt):
    result = list(map(dict, itertools.combinations(dictt.items(), 2)))
    return result    

students = {'V' : [1, 4, 6, 10], 'VI' : [1, 4, 12], 'VII' : [1, 3, 8]} 
print("\nOriginal Dictionary:")
print(students)
print("\nCombinations of key-value pairs of the said dictionary:")
print(test(students))

students = {'V' : [1, 3, 5], 'VI' : [1, 5]} 
print("\nOriginal Dictionary:")
print(students)
print("\nCombinations of key-value pairs of the said dictionary:")
print(test(students))


# 59. Write a Python program to find the specified number of maximum values in a given dictionary. 
# Original Dictionary:
# {'a': 5, 'b': 14, 'c': 32, 'd': 35, 'e': 24, 'f': 100, 'g': 57, 'h': 8, 'i': 100}
# 1 maximum value(s) in the said dictionary:
# ['f']
# 2 maximum value(s) in the said dictionary:
# ['f', 'i']
# 5 maximum value(s) in the said dictionary:
# ['f', 'i', 'g', 'd', 'c']
def test(dictt, N):
    result = sorted(dictt, key=dictt.get, reverse=True)[:N]
    return result 
dictt = {'a':5, 'b':14, 'c': 32, 'd':35, 'e':24, 'f': 100, 'g':57, 'h':8, 'i': 100}
print("\nOriginal Dictionary:")
print(dictt)
N = 1
print("\n",N,"maximum value(s) in the said dictionary:")
print(test(dictt, N))
N = 2
print("\n",N,"maximum value(s) in the said dictionary:")
print(test(dictt, N))
N = 5
print("\n",N,"maximum value(s) in the said dictionary:")
print(test(dictt, N))


# 60. Write a Python program to find shortest list of values with the keys in a given dictionary. 
# Original Dictionary: {'V': [10, 12], 'VI': [10], 'VII': [10, 20, 30, 40], 'VIII': [20], 'IX': [10, 30, 50, 70], 'X': [80]} Shortest list of values with the keys of the said dictionary: ['VI', 'VIII', 'X']

def test(dictt):
    min_value=1
    result = [k for k, v in dictt.items() if len(v) == (min_value)] 
    return result    

dictt = {
 'V': [10, 12],
 'VI': [10],
 'VII': [10, 20, 30, 40],
 'VIII': [20],
 'IX': [10,30,50,70],
 'X': [80]
 }

print("\nOriginal Dictionary:")
print(dictt)
print("\nShortest list of values with the keys of the said dictionary:")
print(test(dictt))

# 61. Write a Python program to count the frequency in a given dictionary. 
# Original Dictionary:
# {'V': 10, 'VI': 10, 'VII': 40, 'VIII': 20, 'IX': 70, 'X': 80, 'XI': 40, 'XII': 20}
# Count the frequency of the said dictionary:
# Counter({10: 2, 40: 2, 20: 2, 70: 1, 80: 1})

from collections import Counter
def test(dictt):    
    result = Counter(dictt.values())
    return result    

dictt = {
 'V': 10,
 'VI': 10,
 'VII': 40,
 'VIII': 20,
 'IX': 70,
 'X': 80,
 'XI': 40,
 'XII': 20, 
 }

print("\nOriginal Dictionary:")
print(dictt)
print("\nCount the frequency of the said dictionary:")
print(test(dictt))

# 62. Write a Python program to extract values from a given dictionaries and create a list of lists from those values. 
# Original Dictionary:
# [{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]
# Extract values from the said dictionarie and create a list of lists using those values:
# [[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]
# [[1, 'Jean Castro'], [2, 'Lula Powell'], [3, 'Brian Howell'], [4, 'Lynne Foster'], [5, 'Zachary Simon']]
# [['Jean Castro', 'V'], ['Lula Powell', 'V'], ['Brian Howell', 'VI'], ['Lynne Foster', 'VI'], ['Zachary Simon', 'VII']]
def test(dictt,keys):
    return [list(d[k] for k in keys) for d in dictt] 

students = [
        {'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, 
        {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'},
        {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, 
        {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, 
        {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}
        ]

print("\nOriginal Dictionary:")
print(students)
print("\nExtract values from the said dictionarie and create a list of lists using those values:")
print("\n",test(students,('student_id', 'name', 'class')))
print("\n",test(students,('student_id', 'name')))
print("\n",test(students,('name', 'class')))


# 63. Write a Python program to convert a given list of lists to a dictionary. 
# Original list of lists:
# [[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]
# Convert the said list of lists to a dictionary:
# {1: ['Jean Castro', 'V'], 2: ['Lula Powell', 'V'], 3: ['Brian Howell', 'VI'], 4: ['Lynne Foster', 'VI'], 5: ['Zachary Simon', 'VII']}
def test(lst):
    result = {item[0]: item[1:] for item in lst}
    return result

students = [[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]

print("\nOriginal list of lists:")
print(students)
print("\nConvert the said list of lists to a dictionary:")
print(test(students))


# 64. Write a Python program to create a key-value list pairings in a given dictionary. 
# Original dictionary:
# {1: ['Jean Castro'], 2: ['Lula Powell'], 3: ['Brian Howell'], 4: ['Lynne Foster'], 5: ['Zachary Simon']}
# A key-value list pairings of the said dictionary:
# [{1: 'Jean Castro', 2: 'Lula Powell', 3: 'Brian Howell', 4: 'Lynne Foster', 5: 'Zachary Simon'}]

from itertools import product
def test(dictt):
    result = [dict(zip(dictt, sub)) for sub in product(*dictt.values())]
    return result

students = {1: ['Jean Castro'], 2: ['Lula Powell'], 3: ['Brian Howell'], 4: ['Lynne Foster'], 5: ['Zachary Simon']}

print("\nOriginal dictionary:")
print(students)
print("\nA key-value list pairings of the said dictionary:")
print(test(students))

# 65. Write a Python program to get the total length of all values of a given dictionary with string values. 
# Original dictionary:
# {'#FF0000': 'Red', '#800000': 'Maroon', '#FFFF00': 'Yellow', '#808000': 'Olive'}
# Total length of all values of the said dictionary with string values:
# 20
def test(dictt):
    result = sum((len(values) for values in dictt.values()))
    return result
color = {'#FF0000':'Red', '#800000':'Maroon', '#FFFF00':'Yellow', '#808000':'Olive'}
print("\nOriginal dictionary:")
print(color)
print("\nTotal length of all values of the said dictionary with string values:")
print(test(color))


# 66. Write a Python program to check if a specific Key and a value exist in a dictionary. 
# Original dictionary:
# [{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]
# Check if a specific Key and a value exist in the said dictionary:
# True
# True
# True
# False
# False
# False
def test(dictt, key, value):
   if any(sub[key] == value for sub in dictt):
       return True
   return False

students = [
        {'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, 
        {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'},
        {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, 
        {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, 
        {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}
        ]


print("\nOriginal dictionary:")
print(students)
print("\nCheck if a specific Key and a value exist in the said dictionary:")
print(test(students,'student_id', 1))
print(test(students,'name', 'Brian Howell'))
print(test(students,'class', 'VII'))
print(test(students,'class', 'I'))
print(test(students,'name', 'Brian Howelll'))
print(test(students,'student_id', 11))


# 67. Write a Python program to invert a given dictionary with non-unique hashable values. 
# Sample Output:
# {8: ['Ora Mckinney', 'Mathew Gilbert'], 7: ['Theodore Hollandl', 'Mae Fleming', 'Ivan Little']}

from collections import defaultdict
def test(students):
  obj = defaultdict(list)
  for key, value in students.items():
    obj[value].append(key)
  return dict(obj)
 
students = {
  'Ora Mckinney': 8,
  'Theodore Hollandl': 7,
  'Mae Fleming': 7,
  'Mathew Gilbert': 8,
  'Ivan Little': 7,  
}
print(test(students))

# 68. Write a Python program to combines two or more dictionaries, creating a list of values for each key. 
# Sample Output:
# Original dictionaries:
# {'w': 50, 'x': 100, 'y': 'Green', 'z': 400}
# {'x': 300, 'y': 'Red', 'z': 600}
# Combined dictionaries, creating a list of values for each key:
# {'w': [50], 'x': [100, 300], 'y': ['Green', 'Red'], 'z': [400, 600]}
from collections import defaultdict
def test(*dicts):
  result = defaultdict(list)
  for el in dicts:
    for key in el:
      result[key].append(el[key])
  return dict(result)
 
d1 = {'w': 50, 'x': 100, 'y': 'Green', 'z': 400}
d2 = {'x': 300, 'y': 'Red', 'z': 600}

print("Original dictionaries:")
print(d1)
print(d2)
print("\nCombined dictionaries, creating a list of values for each key:")
print(test(d1, d2))


# 69. Write a Python program to group the elements of a given list based on the given function. 
# Sample Output:
# Original list & function:
# [7, 23, 3.2, 3.3, 8.4] Function name: floor:
# Group the elements of the said list based on the given function:
# {7: [7], 23: [23], 3: [3.2, 3.3], 8: [8.4]}
# Original list & function:
# ['Red', 'Green', 'Black', 'White', 'Pink'] Function name: len:
# Group the elements of the said list based on the given function:
# {3: ['Red'], 5: ['Green', 'Black', 'White'], 4: ['Pink']}
from collections import defaultdict
from math import floor
def test(lst, fn):
  d = defaultdict(list)
  for el in lst:
    d[fn(el)].append(el)
  return dict(d)
nums = [7,23, 3.2, 3.3, 8.4]
print("Original list & function:")
print(nums," Function name: floor:")
print("Group the elements of the said list based on the given function:")
print(test(nums, floor))
print("\n")
print("Original list & function:")
colors = ['Red', 'Green', 'Black', 'White', 'Pink']
print(colors," Function name: len:")
print("Group the elements of the said list based on the given function:")
print(test(colors, len))


# 70. Write a Python program to map the values of a given list to a dictionary using a function, where the key-value pairs consist of the original value as the key and the result of the function as the value. 
# Sample Output:
# {1: 1, 2: 4, 3: 9, 4: 16}

def test(itr, fn):
  return dict(zip(itr, map(fn, itr)))
print(test([1, 2, 3, 4], lambda x: x * x))

# 71. Write a Python program to retrieve the value of the nested key indicated by the given selector list from a dictionary or list. 
# Sample Output:
# Russell
# 2

from functools import reduce 
from operator import getitem
def test(d, selectors):
  return reduce(getitem, selectors, d) 
users = {
  'Carla ': {
    'name': {
      'first': 'Carla ',
      'last': 'Russell' 
    },
    'postIds': [1, 2, 3, 4, 5]
  }
}
print(test(users, ['Carla ', 'name', 'last']))
print(test(users, ['Carla ', 'postIds', 1]))

# 72. Write a Python program to invert a dictionary with unique hashable values. 
# Sample Output:
# {10: 'Theodore', 11: 'Mathew', 9: 'Roxanne'}

def test(students):
  return { value: key for key, value in students.items() }
 
students = {
  'Theodore': 10,
  'Mathew': 11,
  'Roxanne': 9,
}
print(test(students))

# 73. Write a Python program to convert a list of dictionaries into a list of values corresponding to the specified key. 
# Sample Output:
# Original list of dictionaries:
# [{'name': 'Theodore', 'age': 18}, {'name': 'Mathew', 'age': 22}, {'name': 'Roxanne', 'age': 20}, {'name': 'David', 'age': 18}]
# Convert a list of dictionaries into a list of values corresponding to the specified key:
# [18, 22, 20, 18]

def test(lsts, key):
  return [x.get(key) for x in lsts]
 
students = [
  { 'name': 'Theodore', 'age': 18 },
  { 'name': 'Mathew', 'age': 22 },
  { 'name': 'Roxanne', 'age': 20 },
  { 'name': 'David', 'age': 18 }
]

print("Original list of dictionaries:")
print(students)
print("\nConvert a list of dictionaries into a list of values corresponding to the specified key:")
print(test(students, 'age'))

# 74. Write a Python program to create a dictionary with the same keys as the given dictionary and values generated by running the given function for each value. 
# Sample Output:
# Original dictionary elements:
# {'Theodore': {'user': 'Theodore', 'age': 45}, 'Roxanne': {'user': 'Roxanne', 'age': 15}, 'Mathew': {'user': 'Mathew', 'age': 21}}
# Dictionary with the same keys:
# {'Theodore': 45, 'Roxanne': 15, 'Mathew': 21}
def test(obj, fn):
  return dict((k, fn(v)) for k, v in obj.items())
 
users = {
  'Theodore': { 'user': 'Theodore', 'age': 45 },
  'Roxanne': { 'user': 'Roxanne', 'age': 15 },
  'Mathew': { 'user': 'Mathew', 'age': 21 },
}
print("\nOriginal dictionary elements:")
print(users)
print("\nDictionary with the same keys:")
print(test(users, lambda u : u['age']))


# 75. Write a Python program to find all keys in the provided dictionary that have the given value. 
# Sample Output:
# Original dictionary elements:
# {'Theodore': 19, 'Roxanne': 20, 'Mathew': 21, 'Betty': 20}
# Find all keys in the said dictionary that have the specified value:
# ['Roxanne', 'Betty']
def test(dict, val):
  return list(key for key, value in dict.items() if value == val)

students = {
  'Theodore': 19,
  'Roxanne': 20,
  'Mathew': 21,
  'Betty': 20
}

print("\nOriginal dictionary elements:")
print(students)
print("\nFind all keys in the said dictionary that have the specified value:")
print(test(students, 20))


# 76. Write a Python program to combine two lists into a dictionary, where the elements of the first one serve as the keys and the elements of the second one serve as the values. The values of the first list need to be unique and hashable. 
# Sample Output:
# Original lists:
# ['a', 'b', 'c', 'd', 'e', 'f']
# [1, 2, 3, 4, 5]
# Combine the values of the said two lists into a dictionary:
# {'a': 1, 'b': 2, 'c': 3, 'd': 4, 'e': 5}
def test(keys, values):
  return dict(zip(keys, values))

l1 = ['a', 'b', 'c', 'd', 'e', 'f']
l2 = [1, 2, 3, 4, 5]     
print("Original lists:")
print(l1)
print(l2)
print("\nCombine the values of the said two lists into a dictionary:")
print(test(l1, l2))


# 77. Write a Python program to convert given a dictionary to a list of tuples. 
# Sample Output:
# Original Dictionary:
# {'Red': 1, 'Green': 3, 'White': 5, 'Black': 2, 'Pink': 4}
# Convert the said dictionary to a list of tuples:
# [('Red', 1), ('Green', 3), ('White', 5), ('Black', 2), ('Pink', 4)]

def test(d):
  return list(d.items())
 
d = {'Red': 1, 'Green': 3, 'White': 5, 'Black': 2, 'Pink': 4}
print("Original Dictionary:")
print(d)
print("\nConvert the said dictionary to a list of tuples:")
print(test(d))

# 78. Write a Python program to create a flat list of all the keys in a flat dictionary. 
# Sample Output:
# Original dictionary elements:
# {'Theodore': 19, 'Roxanne': 20, 'Mathew': 21, 'Betty': 20}
# Create a flat list of all the keys of the said flat dictionary:
# ['Theodore', 'Roxanne', 'Mathew', 'Betty']
def test(flat_dict):
  return list(flat_dict.keys())
students = {
  'Theodore': 19,
  'Roxanne': 20,
  'Mathew': 21,
  'Betty': 20
}

print("\nOriginal dictionary elements:")
print(students)
print("\nCreate a flat list of all the keys of the said flat dictionary:")
print(test(students))


# 79. Write a Python program to create a flat list of all the values in a flat dictionary. 
# Sample Output:
# Original dictionary elements:
# {'Theodore': 19, 'Roxanne': 20, 'Mathew': 21, 'Betty': 20}
# Create a flat list of all the values of the said flat dictionary:
# [19, 20, 21, 20]
def test(flat_dict):
  return list(flat_dict.values())
students = {
  'Theodore': 19,
  'Roxanne': 20,
  'Mathew': 21,
  'Betty': 20
}
print("\nOriginal dictionary elements:")
print(students)
print("\nCreate a flat list of all the values of the said flat dictionary:")
print(test(students))


# 80. Write a Python program to find the key of the maximum value in a dictionary. 
# Sample Output:
# Original dictionary elements:
# {'Theodore': 19, 'Roxanne': 22, 'Mathew': 21, 'Betty': 20}
# Finds the key of the maximum and minimum value of the said dictionary:
# ('Roxanne', 'Theodore')
def test(d):
  return max(d, key = d.get), min(d, key = d.get)

students = {
  'Theodore': 19,
  'Roxanne': 22,
  'Mathew': 21,
  'Betty': 20
}
print("\nOriginal dictionary elements:")
print(students)
print("\nFinds the key of the maximum and minimum value of the said dictionary:")
print(test(students))




Enum&Bisect.py from enum import IntEnum
import enum
from enum import Enum
from collections import Counter
from bisect import bisect, bisect_left
from bisect import bisect_right
from bisect import bisect_left
import bisect

NOTE:'''Python Bisect Excercises'''

# 1. Write a Python program to locate the left insertion point for a specified value in sorted order. 
# Expected Output: 4 2
def index(a, x):
    return bisect.bisect_left(a, x)
a = [1, 2, 4, 5]
print(index(a, 6))
print(index(a, 3))

# 2. Write a Python program to locate the right insertion point for a specified value in sorted order. 
# Expected Output: 3 2
def index(a, x):
    return bisect.bisect_right(a, x)
a = [1, 2, 4, 7]
print(index(a, 6))
print(index(a, 3))

# 3. Write a Python program to insert items into a list in sorted order. 
# Expected Output:
# Original List:
# [25, 45, 36, 47, 69, 48, 68, 78, 14, 36]
# Sorted List:
# [14, 25, 36, 36, 45, 47, 48, 68, 69, 78]
import bisect
# Sample list
my_list = [25, 45, 36, 47, 69, 48, 68, 78, 14, 36]
print("Original List:")
print(my_list)
sorted_list = []
for i in my_list:
    position = bisect.bisect(sorted_list, i)
    bisect.insort(sorted_list, i)
print("Sorted List:")
print(sorted_list)

# 4. Write a Python program to find the first occurrence of a given number in a sorted list using Binary Search(bisect). 
# Expected Output:
# First occurrence of 8 is present at index 4
from bisect import bisect_left   
def Binary_Search(a, x): 
    i = bisect_left(a, x)
    return i if i != len(a) and a[i] == x else -1

nums = [1, 2, 3, 4, 8, 8, 10, 12] 
x = 8 
num_position = Binary_Search(nums, x) 
if num_position == -1: 
    print(x, "is not present.") 
else: 
    print("First occurrence of", x, "is present at index", num_position)

# 5. Write a Python program to find the index position of the largest value smaller than a given number in a sorted list using Binary Search(bisect). 
# Expected Output:
# Largest value smaller than 5 is at index 3
def Binary_Search(l, x):
    return (i-1) if (i := bisect_left(l, x)) else -1

nums = [1, 2, 3, 4, 8, 8, 10, 12]
x = 5
num_position = Binary_Search(nums, x)
if num_position == -1:
    print("Not found..!")
else:
    print("Largest value smaller than ", x, " is at index ", num_position)

# 6. Write a Python program to find the index position of the last occurrence of a given number in a sorted list using Binary Search(bisect). 
# Expected Output:
# Last occurrence of 8 is present at 5
def BinarySearch(a, x):
    i = bisect_right(a, x)
    return (i-1) if i != len(a)+1 and a[i-1] == x else -1

nums = [1, 2, 3, 4, 8, 8, 10, 12]
x = 8
num_position = BinarySearch(nums, x)
if num_position == -1:
    print("not presetn!")
else:
    print("Last occurrence of", x, "is present at", num_position)

# 7. Write a Python program to find three integers which gives the sum of zero in a given array of integers using Binary Search(bisect). 
# Expected Output:
# [[-40, 0, 40], [-20, -20, 40], [-20, 0, 20]]
# [[-6, 1, 5], [-6, 2, 4]]
class Solution:
    def three_Sum(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        triplets = []
        if len(nums) < 3:
            return triplets
        num_freq = Counter(nums)
        nums = sorted(num_freq)  # Sorted unique numbers
        max_num = nums[-1]
        for i, v in enumerate(nums):
            if num_freq[v] >= 2:
                complement = -2 * v
                if complement in num_freq and (
                    complement != v or num_freq[v] >= 3
                ):
                    triplets.append([v] * 2 + [complement])

            # When all 3 numbers are different.
            if v < 0:  # Only when v is the smallest
                two_sum = -v

                # Lower/upper bound of the smaller of remainingtwo.
                lb = bisect_left(nums, two_sum - max_num, i + 1)
                ub = bisect(nums, two_sum // 2, lb)
                for u in nums[lb: ub]:
                    complement = two_sum - u
                    if complement in num_freq and u != complement:
                        triplets.append([v, u, complement])
        return triplets

nums = [-20, 0, 20, 40, -20, -40, 80]
s = Solution()
result = s.three_Sum(nums)
print(result)
nums = [1, 2, 3, 4, 5, -6]
result = s.three_Sum(nums)
print(result)

# 8. Write a Python program to find a triplet in an array such that the sum is closest to a given number. Return the sum of the three integers. 
# Expected Output:
# Array values & target value: [1, 2, 3, 4, 5, -6] & 14
# Sum of the integers closest to target: 12
# Array values & target value: [1, 2, 3, 4, -5, -6] & 5
# Sum of the integers closest to target: 6
#Source: https://bit.ly/2SRefdb
class Solution:
    def threeSumClosest(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: int
        """
        nums = sorted(nums)
        # Let top[i] be the sum of largest i numbers.
        top = [
            0,
            nums[-1],
            nums[-1] + nums[-2]
        ]
        min_diff = float('inf')
        three_sum = 0
        # Find range of the least number in curr_n (0, 1, 2 or 3)
        # numbers that sum up to curr_target, then find range of
        # 2nd least number and so on by recursion.

        def closest(curr_target, curr_n, lo=0):
            if curr_n == 0:
                nonlocal min_diff, three_sum
                if abs(curr_target) < min_diff:
                    min_diff = abs(curr_target)
                    three_sum = target - curr_target
                return

            next_n = curr_n - 1
            max_i = len(nums) - curr_n
            max_i = bisect(
                nums, curr_target // curr_n,
                lo, max_i)
            min_i = bisect_left(
                nums, curr_target - top[next_n],
                lo, max_i) - 1
            min_i = max(min_i, lo)

            for i in range(min_i, max_i + 1):
                if min_diff == 0:
                    return
                if i == min_i or nums[i] != nums[i - 1]:
                    next_target = curr_target - nums[i]
                    closest(next_target, next_n, i + 1)

        closest(target, 3)
        return three_sum

s = Solution()
nums = [1, 2, 3, 4, 5, -6]
target = 14
result = s.threeSumClosest(nums, target)
print("\nArray values & target value:", nums, "&", target)
print("Sum of the integers closest to target:", result)

nums = [1, 2, 3, 4, -5, -6]
target = 5
result = s.threeSumClosest(nums, target)
print("\nArray values & target value:", nums, "&", target)
print("Sum of the integers closest to target:", result)

# 9. Write a Python program to find four elements from a given array of integers whose sum is equal to a given number. The solution set must not contain duplicate quadruplets. 
# Expected Output:
# Array values & target value: [-2, -1, 1, 2, 3, 4, 5, 6] & 10
# Solution Set:
# [[-2, 1, 5, 6], [-2, 2, 4, 6], [-2, 3, 4, 5], [-1, 1, 4, 6],
#     [-1, 2, 3, 6], [-1, 2, 4, 5], [1, 2, 3, 4]]
#Source: https://bit.ly/2SSoyhf
from bisect import bisect_left
class Solution:
    def fourSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[List[int]]
        """
        N = 4
        quadruplets = []
        if len(nums) < N:
            return quadruplets
        nums = sorted(nums)
        quadruplet = []

        # Let top[i] be the sum of largest i numbers.
        top = [0]       
        for i in range(1, N):
            top.append(top[i - 1] + nums[-i])

        # Find range of the least number in curr_n (0,...,N)
        # numbers that sum up to curr_target, then find range
        # of 2nd least number and so on by recursion.
        def sum_(curr_target, curr_n, lo=0):
            if curr_n == 0:
                if curr_target == 0:
                    quadruplets.append(quadruplet[:])
                return

            next_n = curr_n - 1
            max_i = len(nums) - curr_n
            max_i = bisect_left(
                nums, curr_target // curr_n,
                lo, max_i)
            min_i = bisect_left(
                nums, curr_target - top[next_n],
                lo, max_i)

            for i in range(min_i, max_i + 1): 
                if i == min_i or nums[i] != nums[i - 1]:
                    quadruplet.append(nums[i])
                    next_target = curr_target - nums[i]
                    sum_(next_target, next_n, i + 1)
                    quadruplet.pop()

        sum_(target, N)
        return quadruplets

s = Solution()
nums = [-2, -1, 1, 2, 3, 4, 5, 6]
target = 10
result = s.fourSum(nums, target)
print("\nArray values & target value:",nums,"&",target)
print("Solution Set:\n", result)

NOTE: '''Python Enum Excercises'''
class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672

print(f'\nMember name: {Country.Albania.name}')
print(f'Member value: {Country.Albania.value}')

class Country(Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672

for data in Country:
    print('{:15} = {}'.format(data.name, data.value))


class Country(enum.IntEnum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672


print('Country Name ordered by Country Code:')
print('\n'.join(f'  {c.name}' for c in sorted(Country)))


class Country(IntEnum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    Antarctica = 672

country_code_list = list(map(int, Country))
print(country_code_list)


# 5.Write a Python program to get the unique enumeration values. 
# Expected Output:
# Afghanistan = 93
# Albania = 355
# Algeria = 213
# Andorra = 376
# Angola = 244
import enum
class Countries(enum.Enum):
    Afghanistan = 93
    Albania = 355
    Algeria = 213
    Andorra = 376
    Angola = 244
    India = 355
    USA = 213
for result in Countries:
    print('{:15} = {}'.format(result.name, result.value))




FileIO.py """
1. Write a Python program to read an entire text file.
"""

# It is good practice to use the with keyword when dealing with file objects.
# The advantage is that the file is properly closed after its suite finishes,
# even if an exception is raised at some point. 
with open('exercises.txt') as f:
    read_data = f.read()
    print(read_data)
print(f.closed)

with open('exercises.txt') as f:
    read_data = f.read()
    print(read_data)
    print(f.closed)
print(f.closed)

"""
2. Write a Python program to read first n lines of a file.
"""

# solution 1
n = 5
with open('exercises.txt') as f:
    for _ in range(n):
        print(f.readline())
# solution 2
n = 5
with open('exercises.txt') as f:
    lines = list(f)
    for line in lines[:n]:
        print(line)

# solution 3
n = 5
with open('exercises.txt') as f:
    lines = f.readlines()
    for line in lines[:n]:
        print(line)

# solution 4
from itertools import islice
n = 5
with open('exercises.txt') as f:
    for line in islice(f, n):
        print(line)

"""
3. Write a Python program to append text to a file and display the text.
"""

with open('exercises.txt', 'a') as f:
    f.write('\nappending a line at the end of the file')
with open('exercises.txt', 'r') as f:
    print(f.read())
"""
4. Write a Python program to read last n lines of a file.
"""

n = 6
with open('exercises.txt') as f:
    lines = list(f)
    for line in lines[-n:]:
        print(line)
"""
5. Write a Python program to read a file line by line and store it into a list.
"""

with open('exercises.txt') as f:
    lines = list(f)
    print(lines)

"""
6. Write a Python program to read a file line by line store it into a variable.
"""

with open('exercises.txt') as f:
    for line in f:
        my_string = line
        print(my_string)

"""
8. Write a python program to find the longest words.
"""

# solution 1 (finds 1 longest word)
with open('exercises.txt') as f:
    lines = list(f)
    longest = ''
    for line in lines:
        for word in line.split():
            if len(longest) < len(word):
                longest = word
    print(longest)

# solution 2 (finds 1 longest word)
with open('exercises.txt') as f:
    words = f.read().split()
    longest = max(words, key=len)
    print(longest)

# solution 3 (finds all words with greatest length)
with open('exercises.txt') as f:
    words = f.read().split()
    max_length = len(max(words, key=len))
    all_longest = [word for word in words if len(word) == max_length]
    print(set(all_longest))

"""
9. Write a Python program to count the number of lines in a text file.
"""

# solution 1
with open('exercises.txt') as f:
    print(len(list(f)))

# solution 2
with open('exercises.txt') as f:
    counter = sum(1 for _ in f)
    print(counter)

"""
10. Write a Python program to count the frequency of words in a file.
"""

# solution 1
import string

with open('exercises.txt') as f:
    # remove punctuation marks from the text 
    intab = string.punctuation
    outtab = " "*len(string.punctuation)
    trans_table = str.maketrans(intab, outtab)
    text = f.read().translate(trans_table)

    # create list of words (all words lowercased)
    words = text.lower().split()

    # create dictionary and populate it with count for each word
    words_freq = {}
    for word in words:
        if word in words_freq:
            words_freq[word] += 1
        else:
           words_freq[word] = 1

print(words_freq)

# solution 2
from collections import Counter
import string

with open('exercises.txt') as f:
    # remove punctuation marks from the text
    intab = string.punctuation
    outtab = " "*len(string.punctuation)
    trans_table = str.maketrans(intab, outtab)
    text = f.read().translate(trans_table)

    # split text to list of words (all lowercased)
    words = text.lower().split()

    # create a Counter object
    words_freq = Counter(words)

print(words_freq)

"""
11. Write a Python program to get the file size of a plain file.
"""
from os import stat

# os.stat() - Get the status of a file or a file descriptor.
# st_size - Size of the file in bytes
statinfo = stat('exercises.txt')
print(statinfo.st_size)

"""
12. Write a Python program to write a list to a file.
"""
my_list = [1, 2, 3, 4]
with open('list-to-file.txt', 'w+') as f:
    f.write(' '.join(map(str, my_list)))

"""
13. Write a Python program to copy the contents of a file to another file.
"""

with open('list-to-file.txt') as f:
    f1 = open('exercises.txt', 'a')
    f1.write(f.read())
f1.close()

# solution 2 (copy with deleting all previous data in target file)
import shutil
shutil.copyfile('list-to-file.txt', 'exercises.txt')

"""
14. Write a Python program to combine each line from first file with the
corresponding line in second file.
"""

with open('fruits.txt') as f1:
    f2 = open('vegetables.txt')
    for line1, line2 in zip(f1, f2):
        print(line1.strip('\n'), line2.strip('\n'))
f2.close()

"""
15. Write a Python program to read a random line from a file.
"""

import random
with open('fruits.txt') as f:
    print(random.choice(f.readlines()))

"""
16. Write a Python program to assess if a file is closed or not.
"""
with open('fruits.txt') as f:
    print(f.closed)
print(f.closed)

"""
17. Write a Python program to remove newline characters from a file.
"""

with open('fruits.txt') as f:
    my_string = ' '.join(map(lambda x: x.strip('\n'), f.readlines()))
    print(my_string)

    f.seek(0)
    print(f.read())

# 18. Write a Python program that takes a text file as input and returns the number of words of a given text file. Go to the editor
def count_words(filepath):
   with open(filepath) as f:
       data = f.read()
       data.replace(",", " ")
       return len(data.split(" "))
print(count_words("words.txt"))

# 19. Write a Python program to extract characters from various text files and puts them into a list. 
import glob
char_list = []
files_list = glob.glob("*.txt")
for file_elem in files_list:
   with open(file_elem, "r") as f:
       char_list.append(f.read())
print(char_list)

# 20. Write a Python program to generate 26 text files named A.txt, B.txt, and so on up to Z.txt. 
import string, os
if not os.path.exists("letters"):
   os.makedirs("letters")
for letter in string.ascii_uppercase:
    with open(f"{letter}.txt", "w") as f:
        f.writelines(letter)

# 21. Write a Python program to create a file where all letters of English alphabet are listed by specified number of letters on each line. 
import string
def letters_file_line(n):
   with open("words1.txt", "w") as f:
       alphabet = string.ascii_uppercase
       letters = [alphabet[i:i + n] + "\n" for i in range(0, len(alphabet), n)]
       f.writelines(letters)
letters_file_line(3)



Functions.py """
4. Write a Python program to reverse a string.
Sample String : "1234abcd"
Expected Output : "dcba4321"
"""

def reverse_string(my_string):
    return ''.join(my_string[::-1])

my_string = input('please enter a string: ')
print('the reversed string is:', reverse_string(my_string))

"""
5. Write a Python function to calculate the factorial of a number
(a non-negative integer). The function accepts the number as an argument.
"""
# solution 1
def factorial(number):
    fact = 1
    for i in range(1, number+1):
        fact *= i
    return fact

my_number = int(input('please enter a positive integer: '))
print('the factorial of your integer is:', factorial(my_number))

# solution 2
def factorial(number):
    return number*(factorial(number-1)) if number > 1 else 1

my_number = int(input('please enter a positive integer: '))
print('the factorial of your integer is:', factorial(my_number))

"""
6.Write a Python function to check whether a number is in a given range.
"""

def is_in_range(number, n, m):
    return number in range(n,m+1)

num = int(input('please enter a number: '))
n = 0
m = 10
print(num, 'is in range', n, '-', m, is_in_range(num, n, m))

"""
8. Write a Python function that takes a list and returns a new list
with unique elements of the first list. 
Sample List : [1,2,3,3,3,3,4,5]
Unique List : [1, 2, 3, 4, 5]
"""

def uniques(my_list):
    uniques =[]
    for i in my_list:
        if i not in uniques:
            uniques.append(i)
    return uniques

sample_list = [1,2,3,3,3,3,4,5]
print(uniques(sample_list))

"""
9. Write a Python function that takes a number as a parameter and check
the number is prime or not. 
Note : A prime number (or a prime) is a natural number greater than 1
and that has no positive divisors other than 1 and itself.
"""

def is_prime(number):
    return True if number == 1 else all(number%i != 0 for i in range(2, number))

my_number = int(input('please enter a number: '))

if is_prime(my_number):
    print('your number is prime')
else:
    print('your number is not prime')

"""
11. Write a Python function to check whether a number is perfect or not.
According to Wikipedia : In number theory, a perfect number is a positive
integer that is equal to the sum of its proper positive divisors, that is,
the sum of its positive divisors excluding the number itself (also known as
its aliquot sum). Equivalently, a perfect number is a number that is half
the sum of all of its positive divisors (including itself).
Example : The first perfect number is 6, because 1, 2, and 3 are its proper
positive divisors, and 1 + 2 + 3 = 6. Equivalently, the number 6 is equal to
half the sum of all its positive divisors: ( 1 + 2 + 3 + 6 ) / 2 = 6.
The next perfect number is 28 = 1 + 2 + 4 + 7 + 14. This is followed by
the perfect numbers 496 and 8128.
"""

def is_perfect(number):
    divisors_sum = sum(i for i in range(1, int(number/2)+1) if number%i == 0)
    return divisors_sum == number

print(is_perfect(6))
print(is_perfect(10))
print(is_perfect(28))
print(is_perfect(30))

"""
13. Write a Python function that prints out the first n rows of Pascal's
triangle. 
Note : Pascal's triangle is an arithmetic and geometric figure first imagined
by Blaise Pascal. In Pascal's triangle each number is the two numbers above
it added together
"""
    
def pascals_triangle(n):
    one = [1]
    print(one)
    for i in range(n):
        row = [row[x]+row[x+1] for x in range(i)]
        row = one + row + one
        print(row)
        
pascals_triangle(5)       

"""
14. Write a Python function to check whether a string is a pangram or not.
Note : Pangrams are words or sentences containing every letter of the
alphabet at least once.
For example : "The quick brown fox jumps over the lazy dog"
"""

def is_pangram(your_string):
    alphabet = ''
    for letter in your_string.lower():
        if letter.isalpha() and letter not in alphabet:
            alphabet += letter
    alphabet = ''.join(sorted(alphabet))
    return alphabet == 'abcdefghijklmnopqrstuvwxyz'

print(is_pangram("The quick brown fox jumps over the lazy dog"))

"""
15. Write a Python program that accepts a hyphen-separated sequence of words as input and prints the words in a hyphen-separated sequence after sorting them alphabetically. Go to the editor
Sample Items : green-red-yellow-black-white
Expected Result : black-green-red-white-yellow
"""

def sort_hyphen(your_string):
    your_list = your_string.split('-')
    your_list.sort()
    return '-'.join(your_list)

my_string = 'green-red-yellow-black-white'
print(sort_hyphen(my_string))

"""
17. Write a Python program to make a chain of function decorators
(bold, italic, underline etc.) in Python.
"""

def bold_decorator(func):
    def wrapper(your_string):
        return '<strong>{}<strong>'.format(func(your_string))
    return wrapper

def paragraph_decorator(func):
    def wrapper(your_string):
        return '<p>{}<p>'.format(func(your_string))
    return wrapper

def div_decorator(func):
    def wrapper(your_string):
        return '<div>{}<div>'.format(func(your_string))
    return wrapper

@bold_decorator
@paragraph_decorator
@div_decorator
def s_to_dollarsign(your_string):
    return ''.join('$' if letter == 's' else letter for letter in your_string)

print(s_to_dollarsign('Despite their heavy build and awkward gait, \nbears are adept runners, climbers, and swimmers.'))

"""
18. Write a Python program to execute a string containing Python code.
"""

my_string = 'print("this is my string")'
exec(my_string)

def my_function():
    print('this is my function')
my_code_object = my_function.__code__
exec(my_code_object)

"""
19. Write a Python program to access a function inside a function.
"""

def my_function(num):
    print('your number is', num)
    def my_nested_func(s):
        print(f'{s} ' * num)

    return my_nested_func

my_number = int(input('enter a number from 0 to 10: '))
my_string = input('enter a word: ')
s = my_function(my_number)
s(my_string)

"""
20. Write a Python program to detect the number of local variables
declared in a function. 
"""

def my_function(number):
    a = 4
    b = 'google'
    x = []
    for _ in range(number):
        print(f'{b} is awesome and a equals {a}')

my_function(2)
print(my_function.__code__.co_nlocals)
print(my_function.__code__.co_varnames)




HeapQueue.py # 1. Write a Python program to find the three largest integers from a given list of numbers using Heap queue algorithm. 
import queue
from heapq import merge
from io import StringIO
import math
from collections import Counter
from pprint import pprint
import heapq
import heapq as hq
nums_list = [25, 35, 22, 85, 14, 65, 75, 22, 58]
print("Original list:")
print(nums_list)
# Find three largest values
largest_nums = hq.nlargest(3, nums_list)
print("\nThree largest numbers are:", largest_nums)

# 2. Write a Python program to find the three smallest integers from a given list of numbers using Heap queue algorithm. 
nums_list = [25, 35, 22, 85, 14, 65, 75, 22, 58]
print("Original list:")
print(nums_list)
# Find three smallest values
largest_nums = hq.nsmallest(3, nums_list)
print("\nThree smallest numbers are:", largest_nums)

# 3. Write a Python program to implement a heapsort by pushing all values onto a heap and then popping off the smallest values one at a time. 
import heapq as hq
def heapsort(iterable):
    h = []
    for value in iterable:
        hq.heappush(h, value)
    return [hq.heappop(h) for _ in range(len(h))]
print(heapsort([1, 3, 5, 7, 9, 2, 4, 6, 8, 0]))

# 4. Write a Python function which accepts an arbitrary list and converts it to a heap using Heap queue algorithm. 
import heapq as hq
raw_heap = [25, 44, 68, 21, 39, 23, 89]
print("Raw Heap: ", raw_heap)
hq.heapify(raw_heap)
print("\nHeapify(heap): ", raw_heap)

# 5. Write a Python program to delete the smallest element from the given Heap and then inserts a new item. 
heap = [25, 44, 68, 21, 39, 23, 89]
hq.heapify(heap)
print("heap: ", heap)
hq.heapreplace(heap, 21)
print("heapreplace(heap, 21): ", heap)
hq.heapreplace(heap, 110)
print("heapreplace(heap, 110): ", heap)

# 6. Write a Python program to sort a given list of elements in ascending order using Heap queue algorithm. 
import heapq as hq
nums_list = [18, 14, 10, 9, 8, 7, 9, 3, 2, 4, 1]
print("Original list:")
print(nums_list)
hq.heapify(nums_list)
s_result = [hq.heappop(nums_list) for _ in range(len(nums_list))]
print("\nSorted list:")
print(s_result)

# 7. Write a Python program to find the kth(1 <= k <= array's length) largest element in an unsorted array using Heap queue algorithm.
class Solution(object):
    def find_Kth_Largest(self, nums, k):
        """
        :type nums: List[int]
        :type of k: int
        :return value type: int
        """
        h = []
        for e in nums:
            heapq.heappush(h, (-e, e))
        for i in range(k):
            w, e = heapq.heappop(h)
            if i == k - 1:
                return e
arr_nums = [12, 14, 9, 50, 61, 41]
s = Solution()
result = s.find_Kth_Largest(arr_nums, 3)
print("Third largest element:", result)
result = s.find_Kth_Largest(arr_nums, 2)
print("\nSecond largest element:", result)
result = s.find_Kth_Largest(arr_nums, 5)
print("\nFifth largest element:", result)

# 8. Write a Python program to compute maximum product of three numbers of a given array of integers using Heap queue algorithm. 
def maximumProduct(nums):
    import heapq
    a, b = heapq.nlargest(3, nums), heapq.nsmallest(2, nums)
    return max(a[0] * a[1] * a[2], a[0] * b[0] * b[1])
arr_nums = [12, 74, 9, 50, 61, 41]
print("Original array elements:")
print(arr_nums)
print("Maximum product of three numbers of the said array:")
print(maximumProduct(arr_nums))

# 9. Write a Python program to find the top k integers that occur the most frequently from a given lists of sorted and distinct integers using Heap queue algorithm. 
def func(nums, k):
    import collections
    d = collections.defaultdict(int)
    for row in nums:
        for i in row:
            d[i] += 1
    temp = []
    import heapq
    for key, v in d.items():
        if len(temp) < k:
            temp.append((v, key))
            if len(temp) == k:
                heapq.heapify(temp)
        elif v > temp[0][0]:
            heapq.heappop(temp)
            heapq.heappush(temp, (v, key))
    result = []
    while temp:
        v, key = heapq.heappop(temp)
        result.append(key)
    return result
nums = [
      [1, 2, 6],
      [1, 3, 4, 5, 7, 8],
      [1, 3, 5, 6, 8, 9],
      [2, 5, 7, 11],
      [1, 4, 7, 8, 12]
    ]  
k = 3
print("Original lists:")
print(nums)
print("\nTop 3 integers that occur the most frequently in the said lists:")
print(func(nums, k))

# 10. Write a Python program to get the n expensive and cheap price items from a given dataset using Heap queue algorithm.
items = [
    {'name': 'Item-1', 'price': 101.1},
    {'name': 'Item-2', 'price': 555.22},
    {'name': 'Item-3', 'price': 45.09},
    {'name': 'Item-4', 'price': 22.75},
    {'name': 'Item-5', 'price': 16.30},
    {'name': 'Item-6', 'price': 110.65}
]

cheap = heapq.nsmallest(3, items, key=lambda s: s['price'])
expensive = heapq.nlargest(3, items, key=lambda s: s['price'])
print("Original datasets:")
pprint(items)
print("\nFirst 3 expensive items:")
pprint(expensive)
print("\nFirst 3 cheap items:")
pprint(cheap)

# 11. Write a Python program to merge multiple sorted inputs into a single sorted iterator(over the sorted values) using Heap queue algorithm. 
num1 = [25, 24, 15, 4, 5, 29, 110]
num2 = [19, 20, 11, 56, 25, 233, 154]
num3 = [24, 26, 54, 48]
num1 = sorted(num1)
num2 = sorted(num2)
num3 = sorted(num3)
result = heapq.merge(num1, num2, num3)
print(list(result))

# 12. Given a n x n matrix where each of the rows and columns are sorted in ascending order, write a Python program to find the kth smallest element in the matrix. 
class Solution(object):
    def find_Kth_Smallest(self, matrix, k):
        """
        :type matrix: List[List[int]]
        :type k: int
        :rtype: int
        """
        m, n = len(matrix), len(matrix[0])
        temp = [[False] * n for _ in range(m)]
        q = [(matrix[0][0], 0, 0)]
        ans = None
        temp[0][0] = True
        for _ in range(k):
            ans, i, j = heapq.heappop(q)
            if i + 1 < m and not temp[i + 1][j]:
                temp[i + 1][j] = True
                heapq.heappush(q, (matrix[i + 1][j], i + 1, j))
            if j + 1 < n and not temp[i][j + 1]:
                temp[i][j + 1] = True
                heapq.heappush(q, (matrix[i][j + 1], i, j + 1))
        return ans
matrix = [
    [0, 5, 9],
    [11, 12, 13],
    [15, 17, 19]
]
k = 1
s = Solution()
result = s.find_Kth_Smallest(matrix, k)
print("First smallest element:", result)
k = 2
s = Solution()
result = s.find_Kth_Smallest(matrix, k)
print("\nSecond smallest element:", result)
k = 3
s = Solution()
result = s.find_Kth_Smallest(matrix, k)
print("\nThird smallest element:", result)

# 13. Write a Python program to find the nth super ugly number from a given prime list of size k using Heap queue algorithm. 
#Ref.: https://bit.ly/32c9P3A
def nth_Super_Ugly_Number(n, primes):
    uglies = [1]

    def gen(prime):
        for ugly in uglies:
            yield ugly * prime

    merged = heapq.merge(*map(gen, primes))
    while len(uglies) < n:
        ugly = next(merged)
        if ugly != uglies[-1]:
            uglies.append(ugly)
    return uglies[-1]
n = 12
primes = [2, 7, 13, 19]
print(nth_Super_Ugly_Number(n, primes))

# 14. Write a Python program to get the k most frequent elements from a given non-empty list of words using Heap queue algorithm.
class Solution:
    def top_K_Frequent(self, words, k):
        """
        :type words: List[str]
        :type k: int
        :return type: List[str]
        """
        ctr = Counter(words)
        heap = [(-ctr[word], word) for word in ctr]
        heapq.heapify(heap)
        return [heapq.heappop(heap)[1] for _ in range(k)]
if __name__ == '__main__':
    words = ["a", "abc", "abcdef", "a", "abcd", "abcd", "abc", "abcdefg"]
    k = 3
    s = Solution()
    print("3 most frequent elements:")
    print(s.top_K_Frequent(words, k))

# 15. Write a Python program to check if the letters of a given string can be rearranged so that two characters that are adjacent to each other are different using Heap queue algorithm. 
import heapq
from collections import Counter
def reorganizeString(S):
    ctr = Counter(S)
    heap = [(-value, key) for key, value in ctr.items()]
    heapq.heapify(heap)
    if (-heap[0][0]) * 2 > len(S) + 1: 
        return ""
    ans = []
    while len(heap) >= 2:
        nct1, char1 = heapq.heappop(heap)
        nct2, char2 = heapq.heappop(heap)
        ans.extend([char1, char2])
        if nct1 + 1: heapq.heappush(heap, (nct1 + 1, char1))
        if nct2 + 1: heapq.heappush(heap, (nct2 + 1, char2))
    return "".join(ans) + (heap[0][1] if heap else "")

print(reorganizeString("aab"))
print(reorganizeString("abc"))
print(reorganizeString("aabb"))
print(reorganizeString("abccdd"))

# 16. Write a Python program which add integer numbers from the data stream to a heapq and compute the median of all elements. Use Heap queue algorithm. 
import heapq
class Median_Finder:
    
    def __init__(self):
        #initialize data structure
        self.max_heap = []
        self.min_heap = []
        

    def add_Number(self, num):
        #type num: int, rtype: void
        if not self.max_heap and not self.min_heap:
            heapq.heappush(self.min_heap, num)
            return
        if not self.max_heap:
            if num > self.min_heap[0]:
                heapq.heappush(self.max_heap, -heapq.heappop(self.min_heap))
                heapq.heappush(self.min_heap, num)
            else:
                heapq.heappush(self.max_heap, -num)
            return
        if len(self.max_heap) == len(self.min_heap):
            if num < -self.max_heap[0]:
                heapq.heappush(self.max_heap, -num)
            else:
                heapq.heappush(self.min_heap, num)
        elif len(self.max_heap) > len(self.min_heap):
            if num < -self.max_heap[0]:
                heapq.heappush(self.min_heap, -heapq.heappop(self.max_heap))
                heapq.heappush(self.max_heap, -num)
            else:
                heapq.heappush(self.min_heap, num)
        elif num > self.min_heap[0]:
            heapq.heappush(self.max_heap, -heapq.heappop(self.min_heap))
            heapq.heappush(self.min_heap, num)
        else:
            heapq.heappush(self.max_heap, -num)
        

    def find_Median(self):
        #rtype: float
        if len(self.max_heap) == len(self.min_heap):
            return (-self.max_heap[0] + self.min_heap[0]) / 2
        elif len(self.max_heap) > len(self.min_heap):
            return -self.max_heap[0]
        else:
            return self.min_heap[0] 
S = Median_Finder()
S.add_Number(1)
S.add_Number(2)
result = S.find_Median()
print(result)
S.add_Number(3)
result = S.find_Median()
print(result)
S.add_Number(4)
S.add_Number(5)
result = S.find_Median()
print(result)

# 17. You are given two integer arrays sorted in ascending order and an integer k. Write a Python program to find k number of pairs(u, v) which consists of one element from the first array and one element from the second array using Heap queue algorithm. 


def k_Smallest_Pairs(nums1, nums2, k):
   queue = []

   def push(i, j):
       if i < len(nums1) and j < len(nums2):
           heapq.heappush(queue, [nums1[i] + nums2[j], i, j])
   push(0, 0)
   pairs = []
   while queue and len(pairs) < k:
       _, i, j = heapq.heappop(queue)
       pairs.append([nums1[i], nums2[j]])
       push(i, j + 1)
       if j == 0:
           push(i + 1, 0)
   return pairs


nums1 = [1, 3, 7]
nums2 = [2, 4, 6]
k = 2
result = k_Smallest_Pairs(nums1, nums2, k)
print(result)

# 18. Write a Python program to find the nth ugly number using Heap queue algorithm. 


class Solution(object):
    #:type n: integer
    #:return type: integer
   def nth_Ugly_Number(self, n):
       ugly_num = 0
       heap = []
       heapq.heappush(heap, 1)
       for _ in range(n):
           ugly_num = heapq.heappop(heap)
           if ugly_num % 2 == 0:
               heapq.heappush(heap, ugly_num * 2)
           elif ugly_num % 3 == 0:
               heapq.heappush(heap, ugly_num * 2)
               heapq.heappush(heap, ugly_num * 3)
           else:
               heapq.heappush(heap, ugly_num * 2)
               heapq.heappush(heap, ugly_num * 3)
               heapq.heappush(heap, ugly_num * 5)
               return ugly_num
n = 7
S = Solution()
result = S.nth_Ugly_Number(n)
print("7th Ugly number:")
print(result)
n = 10
result = S.nth_Ugly_Number(n)
print("\n10th Ugly number:")
print(result)

# 19. Write a Python program to print a heap as a tree-like data structure.
#source https://bit.ly/38HXSoU
def show_tree(tree, total_width=60, fill=' '):
    """Pretty-print a tree.
    total_width depends on your input size"""
    output = StringIO()
    last_row = -1
    for i, n in enumerate(tree):
        row = int(math.floor(math.log(i+1, 2))) if i else 0
        if row != last_row:
            output.write('\n')
        columns = 2**row
        col_width = int(math.floor((total_width * 1.0) / columns))
        output.write(str(n).center(col_width, fill))
        last_row = row
    print(output.getvalue())
    print('-' * total_width)
    return
#test
heap = []
heapq.heappush(heap, 1)
heapq.heappush(heap, 2)
heapq.heappush(heap, 3)
heapq.heappush(heap, 4)
heapq.heappush(heap, 7)
heapq.heappush(heap, 9)
heapq.heappush(heap, 10)
heapq.heappush(heap, 8)
heapq.heappush(heap, 16)
heapq.heappush(heap, 14)
show_tree(heap)

# 20. Write a Python program to combine two given sorted lists using heapq module. 
# Sample Output:
# Original sorted lists:
# [1, 3, 5, 7, 9, 11]
# [0, 2, 4, 6, 8, 10]
# After merging the said two sorted lists:
# [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
nums1 = [1, 3, 5, 7, 9, 11]
nums2 = [0, 2, 4, 6, 8, 10]
print("Original sorted lists:")
print(nums1)
print(nums2)
print("\nAfter merging the said two sorted lists:")
print(list(merge(nums1, nums2)))

# 21. Write a Python program to push three items into the heap and print the items from the heap. 
# Sample Output:
# ('V', 1)
# ('V', 2)
# ('V', 3)
import heapq
heap = []
heapq.heappush(heap, ('V', 1))
heapq.heappush(heap, ('V', 2))
heapq.heappush(heap, ('V', 3))
for a in heap:
	print(a)

# 22. Write a Python program to push three items into a heap and return the smallest item from the heap. Also Pop and return the smallest item from the heap. 
# Sample Output:
# Items in the heap:
# ('V', 1)
# ('V', 3)
# ('V', 2)
# ----------------------
# The smallest item in the heap:
# ('V', 1)
# ----------------------
# Pop the smallest item in the heap:
# ('V', 2)
# ('V', 3)
heap = []
heapq.heappush(heap, ('V', 3))
heapq.heappush(heap, ('V', 2))
heapq.heappush(heap, ('V', 1))
print("Items in the heap:")
for a in heap:
	print(a)
print("----------------------")
print("The smallest item in the heap:")
print(heap[0])
print("----------------------")
print("Pop the smallest item in the heap:")
heapq.heappop(heap)
for a in heap:
	print(a)

# 23. Write a Python program to push an item on the heap, then pop and return the smallest item from the heap. 
# Sample Output:
# Items in the heap:
# ('V', 1)
# ('V', 3)
# ('V', 2)
# ----------------------
# Using heappushpop push item on the heap and return the smallest item.
# ('V', 2)
# ('V', 3)
# ('V', 6)
heap = []
heapq.heappush(heap, ('V', 3))
heapq.heappush(heap, ('V', 2))
heapq.heappush(heap, ('V', 1))
print("Items in the heap:")
for a in heap:
	print(a)
print("----------------------")
print("Using heappushpop push item on the heap and return the smallest item.")
heapq.heappushpop(heap, ('V', 6))
for a in heap:
	print(a)

# 24. Write a Python program to create a heapsort, pushing all values onto a heap and then popping off the smallest values one at a time. 
# Sample Output:
# [10, 20, 20, 40, 50, 50, 60, 70, 80, 90, 100]
import heapq
def heapsort(h):
    heap = []
    for value in h:
        heapq.heappush(heap, value)
    return [heapq.heappop(heap) for _ in range(len(heap))]
print(heapsort([10, 20, 50, 70, 90, 20, 50, 40, 60, 80, 100]))

# 25. Write a Python program to get the two largest and three smallest items from a dataset. 
# Sample Output:
# [100, 90][10, 20, 20]
import heapq
h = [10, 20, 50, 70, 90, 20, 50, 40, 60, 80, 100]
print(heapq.nlargest(2,h))
print(heapq.nsmallest(3,h))

# 26. Write a Python program to create a queue and display all the members and size of the queue. 
# Sample Output:
# Members of the queue:
# 0 1 2 3
# Size of the queue:
# 4
q = queue.Queue()
for x in range(4):
    q.put(x)
print("Members of the queue:")
y = z = q.qsize()

for n in list(q.queue):
    print(n, end=" ")
print("\nSize of the queue:")
print(q.qsize())

# 27. Write a Python program to find whether a queue is empty or not. 
# Sample Output:
# True
# False
p = queue.Queue()
q = queue.Queue()
for x in range(4):
    q.put(x)
print(p.empty())
print(q.empty())

# 28. Write a Python program to create a FIFO queue. 
# Sample Output:
# 0 1 2 3
q = queue.Queue()
#insert items at the end of the queue
for x in range(4):
    q.put(str(x))
#remove items from the head of the queue
while not q.empty():
    print(q.get(), end=" ")
print("\n")

# 29. Write a Python program to create a LIFO queue. 
# Sample Output:
# 3 2 1 0
import queue
q = queue.LifoQueue()
#insert items at the head of the queue 
for x in range(4):
    q.put(str(x))
#remove items from the head of the queue 
while not q.empty():
    print(q.get(), end=" ")
print()




Itertools.py # 1. Write a Python program to create an iterator from several iterables in a sequence and display the type and elements of the new iterator. 
from itertools import takewhile
from itertools import combinations
from operator import itemgetter
from itertools import groupby
from more_itertools import windowed
from itertools import permutations
from itertools import product
from itertools import combinations_with_replacement
import itertools
import itertools as it
import operator
from itertools import accumulate
from itertools import chain
def chain_func(l1,l2,l3):
    return chain(l1,l2,l3)    
#List
result = chain_func([1,2,3], ['a','b','c','d'], [4,5,6,7,8,9])
print("Type of the new iterator:")
print(type(result))
print("Elements of the new iterator:")
for i in result:
    print(i)

#Tuple
result = chain_func((10,20,30,40), ('A','B','C','D'), (40,50,60,70,80,90))
print("Type of the new iterator:")
print(type(result))
print("Elements of the new iterator:")
for i in result:
    print(i)

# 2. Write a Python program to generate the running product of the elements of an given iterable.
def running_product(it):
    return accumulate(it, operator.mul)
#List
result = running_product([1, 2, 3, 4, 5, 6, 7])
print("Running product of a list:")
for i in result:
    print(i)
#Tuple
result = running_product((1, 2, 3, 4, 5, 6, 7))
print("Running product of a Tuple:")
for i in result:
    print(i)

# 3. Write a Python program to generate the running maximum, minimum value of the elements of an iterable. 
def running_max_product(iters):
    return accumulate(iters, max)

#List
result = running_max_product([1, 3, 2, 7, 9, 8, 10, 11, 12, 14, 11, 12, 7])
print("Running maximum value of a list:")
for i in result:
    print(i)
#Tuple
result = running_max_product((1, 3, 3, 7, 9, 8, 10, 9, 8, 14, 11, 15, 7))
print("Running maximum value of a Tuple:")
for i in result:
    print(i)


def running_min_product(iters):
    return accumulate(iters, min)

#List
result = running_min_product([3, 2, 7, 9, 8, 10, 11, 12, 1, 14, 11, 12, 7])
print("Running minimum value of a list:")
for i in result:
    print(i)
#Tuple
result = running_min_product((1, 3, 3, 7, 9, 8, 10, 9, 8, 0, 11, 15, 7))
print("Running minimum value of a Tuple:")
for i in result:
    print(i)

# 4. Write a Python program to construct an infinite iterator that returns evenly spaced values starting with a specified number and step.
start = 10
step = 1
print("The starting number is ", start, "and step is ", step)
my_counter = it.count(start, step)
# Following  loop will run for ever
print("The said function print never-ending items:")
for i in my_counter:
    print(i)

# 5. Write a Python program to generate an infinite cycle of elements from an iterable. 
import itertools as it
def cycle_data(iter):
    return it.cycle(iter)
# Following  loops will run for ever    
#List
result = cycle_data(['A','B','C','D'])
print("The said function print never-ending items:")
for i in result:
    print(i)

#String
result = cycle_data('Python itertools')
print("The said function print never-ending items:")
for i in result:
    print(i)

# 6. Write a Python program to make an iterator that drops elements from the iterable as soon as an element is a positive number. 
def drop_while(nums):
    return it.dropwhile(lambda x: x < 0, nums)

nums = [-1, -2, -3, 4, -10, 2, 0, 5, 12]
print("Original list: ", nums)
result = drop_while(nums)
print("Drops elements from the iterable when a positive number arises \n", list(result))
#Alternate solution
def negative_num(x):
    return x < 0

def drop_while(nums):
    return it.dropwhile(negative_num, nums)

nums = [-1, -2, -3, 4, -10, 2, 0, 5, 12]
print("Original list: ", nums)
result = drop_while(nums)
print("Drops elements from the iterable when a positive number arises \n", list(result))

# 7. Write a Python program to make an iterator that drops elements from the iterable as long as the elements are negative
# afterwards, returns every element. 
def drop_while(nums):
    return it.takewhile(lambda x: x < 0, nums)

nums = [-1, -2, -3, 4, -10, 2, 0, 5, 12]
print("Original list: ", nums)
result = drop_while(nums)
print("Drop elements from the said list as long as the elements are negative\n", list(result))
#Alternate solution
def negative_num(x):
    return x < 0

def drop_while(nums):
    return it.dropwhile(negative_num, nums)

nums = [-1, -2, -3, 4, -10, 2, 0, 5, 12]
print("Original list: ", nums)
result = drop_while(nums)
print("Drop elements from the said list as long as the elements are negative\n", list(result))

# 8. Write a Python program to create an iterator that returns consecutive keys and groups from an iterable. 
import itertools as it
print("Iterate over characters of a string and display\nconsecutive keys and groups from the iterable:")
str1 = 'AAAAJJJJHHHHNWWWEERRRSSSOOIIU'
data_groupby = it.groupby(str1)
for key, group in data_groupby:
    print('Key:', key)
    print('Group:', list(group))    
print("\nIterate over elements of a list and display\nconsecutive keys and groups from the iterable:")
str1 = 'AAAAJJJJHHHHNWWWEERRRSSSOOIIU'    
str1 = [1,2,2,3,4,4,5,5,5,6,6,7,7,7,8]
data_groupby = it.groupby(str1)
for key, group in data_groupby:
    print('Key:', key)
    print('Group:', list(group))

# 9. Write a Python program to split an iterable and generate iterables specified number of times. 
def tee_data(iter, n):
    return it.tee(iter, n)
#List
result = tee_data(['A', 'B', 'C', 'D'], 5)
print("Generate iterables specified number of times:")
for i in result:
    print(list(i))

#String
result = tee_data("Python itertools", 4)
print("\nGenerate iterables specified number of times:")
for i in result:
    print(list(i))

# 10. Write a Python program to create an iterator to get specified number of permutations of elements. 
def permutations_data(iter, length):
    return it.permutations(iter, length)
#List
result = permutations_data(['A', 'B', 'C', 'D'], 3)
print("\nIterator to get specified number of permutations of elements:")
for i in result:
    print(i)

#String
result = permutations_data("Python", 2)
print("\nIterator to get specified number of permutations of elements:")
for i in result:
    print(i)

# 11. Write a Python program to generate combinations of a given length of given iterable. 
def combinations_data(iter, length):
    return it.combinations(iter, length)

#List
result = combinations_data(['A', 'B', 'C', 'D'], 1)
print("\nCombinations of an given iterable of length 1:")
for i in result:
    print(i)

#String
result = combinations_data("Python", 1)
print("\nCombinations of an given iterable of length 1:")
for i in result:
    print(i)

#List
result = combinations_data(['A', 'B', 'C', 'D'], 2)
print("\nCombinations of an given iterable of length 2:")
for i in result:
    print(i)

#String
result = combinations_data("Python", 2)
print("\nCombinations of an given iterable of length 2:")
for i in result:
    print(i)

# 12. Write a Python program to create Cartesian product of two or more given lists using itertools.
def cartesian_product(lists):
    return list(itertools.product(*lists))

ls = [[1, 2], [3, 4]]
print("Original Lists:", ls)
print("Cartesian product of the said lists: ", cartesian_product(ls))
ls = [[1, 2, 3], [3, 4, 5]]
print("\nOriginal Lists:", ls)
print("Cartesian product of the said lists: ", cartesian_product(ls))
ls = [[], [1, 2, 3]]
print("\nOriginal Lists:", ls)
print("Cartesian product of the said lists: ", cartesian_product(ls))
ls = [[1, 2], []]
print("\nOriginal Lists:", ls)
print("Cartesian product of the said lists: ", cartesian_product(ls))


# 13. Write a Python program to chose specified number of colours from three different colours and generate all the combinations with repetitions.
def combinations_colors(l, n):
    return combinations_with_replacement(l, n)


l = ["Red", "Green", "Blue"]
print("Original List: ", l)
n = 1
print("\nn = 1")
print(list(combinations_colors(l, n)))
n = 2
print("\nn = 2")
print(list(combinations_colors(l, n)))
n = 3
print("\nn = 3")
print(list(combinations_colors(l, n)))

# 14. Write a Python program generate permutations of specified elements, drawn from specified values.
def permutations_colors(inp, n):
    for x in product(inp, repeat=n):
        c = ''.join(x)
        print(c, end=', ')

str1 = "Red"
print("Original String: ", str1)
print("Permutations of specified elements, drawn from specified values:")
n = 1
print("\nn = 1")
permutations_colors(str1, n)
n = 2
print("\nn = 2")
permutations_colors(str1, n)
n = 3
print("\nn = 3")
permutations_colors(str1, n)

lst1 = ["Red", "Green", "Black"]
print("\n\nOriginal list: ", lst1)
print("Permutations of specified elements, drawn from specified values:")
n = 1
print("\nn = 1")
permutations_colors(lst1, n)
n = 2
print("\nn = 2")
permutations_colors(lst1, n)
n = 3
print("\nn = 3")
permutations_colors(lst1, n)

# 15. Write a Python program to generate all possible permutations of n different objects. 
import itertools
def permutations_all(l):
    for values in itertools.permutations(l):
        print(values)
permutations_all([1])
print("\n")
permutations_all([1,2])
print("\n")
permutations_all([1,2,3])

# 16. Write a Python program find the sorted sequence from a set of permutations of a given input.
def is_seq_sorted(lst):
  print(lst)
  return all(
      x <= y
      for x, y in windowed(lst, 2)
  )

def permutation_sort(lst):
  return next(
      permutation_seq
      for permutation_seq in permutations(lst)
      if is_seq_sorted(permutation_seq)
  )

print("All the sequences:")
print("\nSorted sequence: ", permutation_sort([12, 10, 9]))

print("\n\nAll the sequences:")
print("\nSorted sequence: ", permutation_sort([2, 3, 1, 0]))


# 17. Write a Python program to read a given string character by character and compress repeated character by storing the length of those character(s).
def encode_str(input_str):
    return [(len(list(n)), m) for m, n in groupby(input_str)]

str1 = "AAASSSSKKIOOOORRRREEETTTTAAAABBBBBBDDDDD"
print("Original string:")
print(str1)
print("Result:")
print(encode_str(str1))

str1 = "jjjjiiiiooooosssnssiiiiwwwweeeaaaabbbddddkkkklll"
print("\nOriginal string:")
print(str1)
print("Result:")
print(encode_str(str1))

# 18. Write a Python program to generate permutations of n items in which successive permutations differ from each other by the swapping of any two items.
DEBUG = False  # like the built-in __debug__
def spermutations(n):
    """permutations by swapping. Yields: perm, sign"""
    sign = 1
    p = [[i, 0 if i == 0 else -1]  # [num, direction]
         for i in range(n)]

    if DEBUG:
        print(' #', p)
    yield tuple(pp[0] for pp in p), sign

    while any(pp[1] for pp in p):  # moving
        i1, (n1, d1) = max(((i, pp) for i, pp in enumerate(p) if pp[1]),
                           key=itemgetter(1))
        sign *= -1
        if d1 == -1:
            # Swap down
            i2 = i1 - 1
            p[i1], p[i2] = p[i2], p[i1]
            # If this causes the chosen element to reach the First or last
            # position within the permutation, or if the next element in the
            # same direction is larger than the chosen element:
            if i2 == 0 or p[i2 - 1][0] > n1:
                # The direction of the chosen element is set to zero
                p[i2][1] = 0
        elif d1 == 1:
            # Swap up
            i2 = i1 + 1
            p[i1], p[i2] = p[i2], p[i1]
            # If this causes the chosen element to reach the first or Last
            # position within the permutation, or if the next element in the
            # same direction is larger than the chosen element:
            if i2 == n - 1 or p[i2 + 1][0] > n1:
                # The direction of the chosen element is set to zero
                p[i2][1] = 0
        if DEBUG:
            print(' #', p)
        yield tuple(pp[0] for pp in p), sign

        for i3, pp in enumerate(p):
            n3, d3 = pp
            if n3 > n1:
                pp[1] = 1 if i3 < i2 else -1
                if DEBUG:
                    print(' # Set Moving')


if __name__ == '__main__':
    from itertools import permutations

    for n in (3, 4):
        print('\nPermutations and sign of %i items' % n)
        sp = set()
        for i in spermutations(n):
            sp.add(i[0])
            print('Permutation: %r Sign: %2i' % i)
            #if DEBUG: raw_input('?')
        # Test
        p = set(permutations(range(n)))
        assert sp == p, 'Two methods of generating permutations do not agree'


# 19. Write a Python program which iterates the integers from 1 to a given number and print "Fizz" for multiples of three, print "Buzz" for multiples of five, print "FizzBuzz" for multiples of both three and five using itertools module. 
#Source:https://bit.ly/30PS62m
import itertools as it
def fizz_buzz(n):
    fizzes = it.cycle([""] * 2 + ["Fizz"])
    buzzes = it.cycle([""] * 4 + ["Buzz"])
    fizzes_buzzes = (fizz + buzz for fizz, buzz in zip(fizzes, buzzes))
    result = (word or n for word, n in zip(fizzes_buzzes, it.count(1)))
    for i in it.islice(result, 100):
        print(i)
n = 50
fizz_buzz(n)

# 20. Write a Python program to find the factorial of a number using itertools module. 
import itertools as it
import operator as op

def factorials_nums(n):
    return list(it.accumulate(it.chain([1], range(1, 1 + n)), op.mul))
    
print("Factorials of 5 :", factorials_nums(5))
print("Factorials of 9 :", factorials_nums(9))

# 21. Write a Python program to find the years where 25th of December be a Sunday between 2000 and 2150. 
'''Days of the week'''
# Source:https://bit.ly/30NoXF8
from datetime import date
from itertools import islice
 
# xmasIsSunday :: Int -> Bool
def xmasIsSunday(y):
    '''True if Dec 25 in the given year is a Sunday.'''
    return date(y, 12, 25).weekday() == 6
 
# main :: IO ()
def main():
    '''Years between 2000 and 2150 with 25 December on a Sunday''' 
    xs = list(filter(
        xmasIsSunday,
        enumFromTo(2000)(2150)
    ))
    total = len(xs)
    print(
        fTable(main.__doc__ + ':\n\n' + '(Total ' + str(total) + ')\n')(
            lambda i: str(1 + i)
        )(str)(index(xs))(
            enumFromTo(0)(total - 1)
        )
    )
# GENERIC -------------------------------------------------
 
# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))
# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return (
        lambda n: None
        if n < 0
        else xs[n]
        if (hasattr(xs, "__getitem__"))
        else next(islice(xs, n, None))
    )
 
 # unlines :: [String] -> String
def unlines(xs):
    '''A single string formed by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)
 
#  FORMATTING ---------------------------------------------
# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    ) 
 # MAIN --
if __name__ == '__main__':
    main()

# 22. Write a Python program to create a 24-hour time format(HH: MM) using 4 given digits. Display the latest time and do not use any digit more than once. 
import itertools
def max_time(nums):
    for i in range(len(nums)):
        nums[i] *= -1
    nums.sort()
    for hr1, hr2, m1, m2 in itertools.permutations(nums):
        hrs = -(10*hr1 + hr2)
        mins = -(10*m1 + m2)
        if 60> mins >=0 and 24 > hrs >=0:
            result = "{:02}:{:02}".format(hrs, mins)
            break
    return result

nums = [1,2,3,4]
print("Original array:",nums)
print("Latest time: ",max_time(nums))

nums = [1,2,4,5]
print("\nOriginal array:",nums)
print("Latest time: ",max_time(nums))

nums = [2,2,4,5]
print("\nOriginal array:",nums)
print("Latest time: ",max_time(nums))

nums = [2,2,4,3]
print("\nOriginal array:",nums)
print("Latest time: ",max_time(nums))

nums = [0,2,4,3]
print("\nOriginal array:",nums)
print("Latest time: ",max_time(nums))

# 23. Write a Python program to find the shortest distance from a specified character in a given string. Return the shortest distances through a list and use itertools module to solve the problem. 
def char_shortest_distancer(str1, char1):
    result = [len(str1)] * len(str1)
    prev_char = -len(str1)
    for i in it.chain(range(len(str1)), reversed(range(len(str1)))):
        if str1[i] == char1:
            prev_char = i
        result[i] = min(result[i], abs(i-prev_char))
    return result


str1 = "w3resource"
chr1 = 'r'
print("Original string:", str1, ": Specified character:", chr1)
print(char_shortest_distancer(str1, chr1))

str1 = "python exercises"
chr1 = 'e'
print("\nOriginal string:", str1, ": Specified character:", chr1)
print(char_shortest_distancer(str1, chr1))

str1 = "JavaScript"
chr1 = 'S'
print("\nOriginal string:", str1, ": Specified character:", chr1)
print(char_shortest_distancer(str1, chr1))


# 24. Write a Python program to find the maximum length of a substring in a given string where all the characters of the substring are same. Use itertools module to solve the problem. 
def max_sub_string(str1):
    return max(len(list(x)) for _, x in itertools.groupby(str1))
str1 = "aaabbccddeeeee"

print("Original string:", str1)
print("Maximum length of a substring with unique characters of the said string:")
print(max_sub_string(str1))

str1 = "c++ exercises"
print("\nOriginal string:", str1)
print("Maximum length of a substring with unique characters of the said string:")
print(max_sub_string(str1))


# 25. Write a Python program to find the first two elements of a given list whose sum is equal to a given value. Use itertools module to solve the problem. 
import itertools as it
def sum_pairs_list(nums, n):
    for num2, num1 in list(it.combinations(nums[::-1], 2))[::-1]:
        if num2 + num1 == n:
            return [num1, num2]

nums = [1,2,3,4,5,6,7]     
n = 10
print("Original list:",nums,": Given value:",n)   
print("Sum of pair equal to ",n,"=",sum_pairs_list(nums,n))

nums = [1,2,-3,-4,-5,6,-7]     
n = -6
print("Original list:",nums,": Given value:",n)   
print("Sum of pair equal to ",n,"=",sum_pairs_list(nums,n))

# 26. Write a Python program to find the nth Hamming number. User itertools module. 
import itertools
from heapq import merge
def nth_hamming_number(n):
    def num_recur():
        last = 1
        yield last
        x, y, z = itertools.tee(num_recur(), 3)
        for n in merge((2 * i for i in x), (3 * i for i in y), (5 * i for i in z)):
            if n != last:
                yield n
                last = n
    result =  itertools.islice(num_recur(), n)
    return list(result)[-1]

print(nth_hamming_number(8))
print(nth_hamming_number(14))
print(nth_hamming_number(17))

# 27. Write a Python program to chose specified number of colours from three different colours and generate the unique combinations.
def unique_combinations_colors(list_data, n):
    return [" and ".join(items) for items in combinations(list_data, r=n)]

colors = ["Red", "Green", "Blue"]
print("Original List: ", colors)
n = 1
print("\nn = 1")
print(list(unique_combinations_colors(colors, n)))
n = 2
print("\nn = 2")
print(list(unique_combinations_colors(colors, n)))
n = 3
print("\nn = 3")
print(list(unique_combinations_colors(colors, n)))


# 28. Write a Python program to find the maximum, minimum aggregation pair in given list of integers. 
def max_aggregate(l_data):
    max_pair = max(combinations(l_data, 2), key=lambda pair: pair[0] + pair[1])
    min_pair = min(combinations(l_data, 2), key=lambda pair: pair[0] + pair[1])
    return max_pair, min_pair

nums = [1, 3, 4, 5, 4, 7, 9, 11, 10, 9]
print("Original list:")
print(nums)
result = max_aggregate(nums)
print("\nMaximum aggregation pair of the said list of tuple pair:")
print(result[0])
print("\nMinimum aggregation pair of the said list of tuple pair:")
print(result[1])

# 29. Write a Python program to interleave multiple lists of the same length. Use itertools module. 
def interleave_multiple_lists(list1, list2, list3):
    return list(itertools.chain(*zip(list1, list2, list3)))

list1 = [100, 200, 300, 400, 500, 600, 700]
list2 = [10, 20, 30, 40, 50, 60, 70]
list3 = [1, 2, 3, 4, 5, 6, 7]
print("Original list:")
print("list1:", list1)
print("list2:", list2)
print("list3:", list3)
print("\nInterleave multiple lists:")
print(interleave_multiple_lists(list1, list2, list3))


# 30. Write a Python program to create non-repeated combinations of Cartesian product of four given list of numbers. 
mums1 = [1, 2, 3, 4]
mums2 = [5, 6, 7, 8]
mums3 = [9, 10, 11, 12]
mums4 = [13, 14, 15, 16]
print("Original lists:")
print(mums1)
print(mums2)
print(mums3)
print(mums4)
print("\nSum of the specified range:")
for i in it.product([tuple(mums1)], it.permutations(mums2), it.permutations(mums3), it.permutations(mums4)):
    print(i)

# 31. Write a Python program to count the frequency of consecutive duplicate elements in a given list of numbers. Use itertools module. 
from itertools import groupby
def count_same_pair(nums):
    return [sum(1 for _ in group) for _, group in groupby(nums)]

nums = [1,1,2,2,2,4,4,4,5,5,5,5]
print("Original lists:")
print(nums)
print("\nFrequency of the consecutive duplicate elements:")
print(count_same_pair(nums))

# 32. Write a Python program to count the frequency of the elements of a given unordered list. 
from itertools import groupby
uno_list = [2,1,3,8,5,1,4,2,3,4,0,8,2,0,8,4,2,3,4,2]
print("Original list:")
print(uno_list)
uno_list.sort()
print(uno_list)
print("\nSort the said unordered list:")
print(uno_list)
print("\nFrequency of the elements of the said unordered list:")
result = [len(list(group)) for key, group in groupby(uno_list)]
print(result)

# 33. Write a Python program to find the pairs of maximum and minimum product from a given list. Use itertools module. 
import itertools as it
def list_max_min_pair(nums):
    result_max = max(it.combinations(nums, 2), key = lambda sub: sub[0] * sub[1])
    result_min = min(it.combinations(nums, 2), key = lambda sub: sub[0] * sub[1])
    return result_max, result_min

nums = [2,5,8,7,4,3,1,9,10,1]   
print("The original list: ") 
print(nums)
print("\nPairs of maximum and minimum product from the said list:")
print(list_max_min_pair(nums))

# 34. Write a Python program to compute the sum of digits of each number of a given list of positive integers. 
from itertools import chain
def sum_of_digits(nums):
    return sum(int(y) for y in (chain(*[str(x) for x in nums])))

nums = [10,2,56]
print("Original tuple: ") 
print(nums)
print("Sum of digits of each number of the said list of integers:")
print(sum_of_digits(nums))

nums = [10,20,4,5,70]
print("\nOriginal tuple: ") 
print(nums)
print("Sum of digits of each number of the said list of integers:")
print(sum_of_digits(nums))

# 35. Write a Python program to get all possible combinations of the elements of a given list using itertools module. 
def combinations_list(list1):
    return [list(itertools.combinations(list1, i)) for i in range(len(list1)+1)]

colors = ['orange', 'red', 'green', 'blue']
print("Original list:")
print(colors)
print("\nAll possible combinations of the said list’s elements:")
print(combinations_list(colors))

# 36. Write a Python program to add two given lists of different lengths, start from right, using itertools module. 
from itertools import zip_longest
def elementswise_right_join(l1, l2):
    return [
        a + b for a, b in zip_longest(reversed(l1), reversed(l2), fillvalue=0)
    ][::-1]

nums1 = [2, 4, 7, 0, 5, 8]
nums2 = [3, 3, -1, 7]
print("\nOriginal lists:")
print(nums1)
print(nums2)
print("\nAdd said two lists from right:")
print(elementswise_right_join(nums1, nums2))

nums3 = [1, 2, 3, 4, 5, 6]
nums4 = [2, 4, -3]
print("\nOriginal lists:")
print(nums3)
print(nums4)
print("\nAdd said two lists from right:")
print(elementswise_right_join(nums3, nums4))

# 37. Write a Python program to add two given lists of different lengths, start from left, using itertools module. 


def elementswise_left_join(l1, l2):
    return [a + b for a, b in zip_longest(l1, l2, fillvalue=0)][::1]

nums1 = [2, 4, 7, 0, 5, 8]
nums2 = [3, 3, -1, 7]
print("\nOriginal lists:")
print(nums1)
print(nums2)
print("\nAdd said two lists from left:")
print(elementswise_left_join(nums1, nums2))

nums3 = [1, 2, 3, 4, 5, 6]
nums4 = [2, 4, -3]
print("\nOriginal lists:")
print(nums3)
print(nums4)
print("\nAdd said two lists from left:")
print(elementswise_left_join(nums3, nums4))


# 38. Write a Python program to interleave multiple given lists of different lengths using itertools module. 
from itertools import chain, zip_longest
def interleave_diff_len_lists(list1, list2, list3, list4):
    return [x for x in chain(*zip_longest(list1, list2, list3, list4)) if x is not None]    
    
nums1 = [2, 4, 7, 0, 5, 8]
nums2 = [2, 5, 8]
nums3 = [0, 1]
nums4 = [3, 3, -1, 7]

print("\nOriginal lists:")
print(nums1)
print(nums2)
print(nums3)
print(nums4)
print("\nInterleave said lists of different lengths:")
print(interleave_diff_len_lists(nums1, nums2, nums3, nums4))

# 39. Write a Python program to get the index of the first element, which is greater than a specified element using itertools module.


def first_index(l1, n):
    return len(list(takewhile(lambda x: x[1] <= n, enumerate(l1))))


nums = [12, 45, 23, 67, 78, 90, 100, 76, 38, 62, 73, 29, 83]
print("Original list:")
print(nums)
n = 73
print("\nIndex of the first element which is greater than", n, "in the said list:")
print(first_index(nums, n))
n = 21
print("\nIndex of the first element which is greater than", n, "in the said list:")
print(first_index(nums, n))
n = 80
print("\nIndex of the first element which is greater than", n, "in the said list:")
print(first_index(nums, n))
n = 55
print("\nIndex of the first element which is greater than", n, "in the said list:")
print(first_index(nums, n))

# 40. Write a Python program to split a given list into specified sized chunks using itertools module. 


def split_list(lst, n):
    lst = iter(lst)
    result = iter(lambda: tuple(islice(lst, n)), ())
    return list(result)


nums = [12, 45, 23, 67, 78, 90, 45, 32, 100, 76, 38, 62, 73, 29, 83]
print("Original list:")
print(nums)
n = 3
print("\nSplit the said list into equal size", n)
print(split_list(nums, n))
n = 4
print("\nSplit the said list into equal size", n)
print(split_list(nums, n))
n = 5
print("\nSplit the said list into equal size", n)
print(split_list(nums, n))

# 41. Write a Python program to find all lower and upper mixed case combinations of a given string. 
import itertools
def combination(str1):
    result = map(''.join, itertools.product(*((c.lower(), c.upper()) for c in str1)))
    return list(result)
st ="abc"
print("Original string:")
print(st)
print("All lower and upper mixed case combinations of the said string:")
print(combination(st))
st ="w3r"
print("\nOriginal string:")
print(st)
print("All lower and upper mixed case combinations of the said string:")
print(combination(st))
st ="Python"
print("\nOriginal string:")
print(st)
print("All lower and upper mixed case combinations of the said string:")
print(combination(st))

# 42. Write a Python program to create group of similar items of a given list. 


def group_similar_items(seq):
    return [list(el) for _, el in it.groupby(seq, lambda x: x.split('_')[0])]


colors = ['red_1', 'red_2', 'green_1',
          'green_2', 'green_3', 'orange_1', 'orange_2']
print("Original list:")
print(colors)
print("\nGroup similar items of the said list:")
print(group_similar_items(colors))

colors = ['red_1', 'green-1', 'green_2', 'green_3', 'orange-1', 'orange_2']
print("\nOriginal list:")
print(colors)
print("\nGroup similar items of the said list:")
print(group_similar_items(colors))




JSON.py # 1. Write a Python program to convert JSON data to Python object. 
import json
json_obj =  '{"Name":"David", "Class":"I", "Age":6 }'
python_obj = json.loads(json_obj)
print("\nJSON data:")
print(python_obj)
print("\nName: ",python_obj["Name"])
print("Class: ",python_obj["Class"])
print("Age: ",python_obj["Age"]) 

# 2. Write a Python program to convert Python object to JSON data. 
import json
# a Python object (dict):
python_obj = {
  "name": "David",
  "class":"I",
  "age": 6  
}
print(type(python_obj))
# convert into JSON:
j_data = json.dumps(python_obj)
# result is a JSON string:
print(j_data)

# 3. Write a Python program to convert Python objects into JSON strings. Print all the values. 
import json
python_dict =  {"name": "David", "age": 6, "class":"I"}
python_list =  ["Red", "Green", "Black"]
python_str =  "Python Json"
python_int =  (1234)
python_float =  (21.34)
python_T =  (True)
python_F =  (False)
python_N =  (None)

json_dict = json.dumps(python_dict)
json_list = json.dumps(python_list)
json_str = json.dumps(python_str)
json_num1 = json.dumps(python_int)
json_num2 = json.dumps(python_float)
json_t = json.dumps(python_T)
json_f = json.dumps(python_F)
json_n = json.dumps(python_N)

print("json dict : ", json_dict)
print("jason list : ", json_list)
print("json string : ", json_str)
print("json number1 : ", json_num1)
print("json number2 : ", json_num2)
print("json true : ", json_t)
print("json false : ", json_f)
print("json null ; ", json_n)

# 4. Write a Python program to convert Python dictionary object (sort by key) to JSON data. Print the object members with   indent level. 
import json
j_str = {'4': 5, '6': 7, '1': 3, '2': 4}
print("Original String:")
print(j_str)
print("\nJSON data:")
print(json.dumps(j_str, sort_keys=True, indent=4))

# 5. Write a Python program to convert JSON encoded data into Python objects. 
import json

jobj_dict =  '{"name": "David", "age": 6, "class": "I"}'
jobj_list =   '["Red", "Green", "Black"]'
jobj_string = '"Python Json"'
jobj_int = '1234'
jobj_float =  '21.34'
python_dict =  json.loads(jobj_dict)
python_list = json.loads(jobj_list)
python_str =  json.loads(jobj_string)
python_int =   json.loads(jobj_int)
python_float = json.loads(jobj_float)

print("Python dictionary: ", python_dict)
print("Python list: ", python_list)
print("Python string: ", python_str)
print("Python integer: ", python_int)
print("Python float: ", python_float)

# 6. Write a Python program to create a new JSON file from an existing JSON file. 
import json

with open('states.json') as f:
  state_data= json.load(f)

for state in state_data['states']:
  del state['area_codes']

with open('new_states.json', 'w') as f:
  json.dump(state_data, f, indent=2)

# 7. Write a Python program to check whether an instance is complex or not. 
import json

def encode_complex(object):
  # check using isinstance method
  if isinstance(object, complex):
      return [object.real, object.imag]
    # raised error if object is not complex
  raise TypeError(f"{repr(object)} is not JSON serialized")

complex_obj = json.dumps(2 + 3j, default=encode_complex)
print(complex_obj) 

# 8. Write a Python program to check whether a JSON string contains complex object or not. 
import json
def is_complex_num(objct):
    if '__complex__' in objct:
        return complex(objct['real'], objct['img'])
    return objct

complex_object =json.loads('{"__complex__": true, "real": 4, "img": 5}', object_hook = is_complex_num)
simple_object =json.loads('{"real": 4, "img": 3}', object_hook = is_complex_num)
print("Complex_object: ",complex_object)
print("Without complex object: ",simple_object)

# 9. Write a Python program to access only unique key value of a Python object. 
import json
python_obj = '{"a":  1, "a":  2, "a":  3, "a": 4, "b": 1, "b": 2}'
print("Original Python object:")
print(python_obj)
json_obj = json.loads(python_obj)
print("\nUnique Key in a JSON object:")
print(json_obj) 




Lambda.py '''
ALL CODE SOLUTIONS BY HIGHNESS_ATHARVA (ATHARVA SHAH) 
'''

# 1. Write a Python program to create a lambda function that adds 15 to a given number passed in as an argument, also create a lambda function that multiplies argument x with argument y and print the result. 
# Sample Output:
# 25
# 48
r = lambda a : a + 15
print(r(10))
r = lambda x, y : x * y
print(r(12, 4))

# 2. Write a Python program to create a function that takes one argument, and that argument will be multiplied with an unknown given number. 
# Sample Output:
# Double the number of 15 = 30
# Triple the number of 15 = 45
# Quadruple the number of 15 = 60
# Quintuple the number 15 = 75
def func_compute(n):
 return lambda x : x * n
result = func_compute(2)
print("Double the number of 15 =", result(15))
result = func_compute(3)
print("Triple the number of 15 =", result(15))
result = func_compute(4)
print("Quadruple the number of 15 =", result(15))
result = func_compute(5)
print("Quintuple the number 15 =", result(15))

# 3. Write a Python program to sort a list of tuples using Lambda.
# Original list of tuples:
# [('English', 88), ('Science', 90), ('Maths', 97), ('Social sciences', 82)]
# Sorting the List of Tuples:
# [('Social sciences', 82), ('English', 88), ('Science', 90), ('Maths', 97)]
subject_marks = [('English', 88), ('Science', 90), ('Maths', 97), ('Social sciences', 82)]
print("Original list of tuples:")
print(subject_marks)
subject_marks.sort(key = lambda x: x[1])
print("\nSorting the List of Tuples:")
print(subject_marks)

# 4. Write a Python program to sort a list of dictionaries using Lambda. 
# Original list of dictionaries :
# [{'make': 'Nokia', 'model': 216, 'color': 'Black'}, {'make': 'Mi Max', 'model': '2', 'color': 'Gold'}, {'make': 'Samsung', 'model': 7, 'color': 'Blue'}]
# Sorting the List of dictionaries :
# [{'make': 'Nokia', 'model': 216, 'color': 'Black'}, {'make': 'Samsung', 'model': 7, 'color': 'Blue'}, {'make': 'Mi Max', 'model': '2', 'color': 'Gold'}]
models = [{'make':'Nokia', 'model':216, 'color':'Black'}, {'make':'Mi Max', 'model':'2', 'color':'Gold'}, {'make':'Samsung', 'model': 7, 'color':'Blue'}]
print("Original list of dictionaries :")
print(models)
sorted_models = sorted(models, key = lambda x: x['color'])
print("\nSorting the List of dictionaries :")
print(sorted_models)

# 5. Write a Python program to filter a list of integers using Lambda. 
# Original list of integers:
# [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# Even numbers from the said list:
# [2, 4, 6, 8, 10]
# Odd numbers from the said list:
# [1, 3, 5, 7, 9]
nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print("Original list of integers: ", nums)

print("\nEven numbers from the said list:")
even_nums = list(filter(lambda x: x%2 == 0, nums))
print(even_nums)
print("\nOdd numbers from the said list:")
odd_nums = list(filter(lambda x: x%2 != 0, nums))
print(odd_nums)

# 6. Write a Python program to square and cube every number in a given list of integers using Lambda. 
# Original list of integers:
# [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# Square every number of the said list:
# [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
# Cube every number of the said list:
# [1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]
nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print("Original list of integers:")
print(nums)
print("\nSquare every number of the said list:")
square_nums = list(map(lambda x: x ** 2, nums))
print(square_nums)
print("\nCube every number of the said list:")
cube_nums = list(map(lambda x: x ** 3, nums))
print(cube_nums)

# 7. Write a Python program to find if a given string starts with a given character using Lambda. 
# Sample Output:
# True
# False
starts_with = lambda x: bool(x.startswith('P'))
print(starts_with('Python'))
starts_with = lambda x: bool(x.startswith('P'))
print(starts_with('Java'))

# 8. Write a Python program to extract year, month, date and time using Lambda. 
# Sample Output:
# 2020-01-15 09:03:32.744178
# 2020
# 1
# 15
# 09:03:32.744178
import datetime
now = datetime.datetime.now()
print(now)
year, month, day, t = lambda x: x.year, lambda x: x.month, lambda x: x.day, lambda x: x.time()
print(year(now))
print(month(now))
print(day(now))
print(t(now))

# 9. Write a Python program to check whether a given string is number or not using Lambda. 
is_num = lambda q: q.replace('.','',1).isdigit()
#Here, we are eliminating . with  '' since it is True in isdigit() checking
print(is_num('26587'))
print(is_num('4.2365'))
print(is_num('-12547'))
print(is_num('00'))
print(is_num('A001'))
print(is_num('001'))
print("\nPrint checking numbers:")
is_num1 = lambda r: is_num(r[1:]) if r[0]=='-' else is_num(r)
print(is_num1('-16.4'))
print(is_num1('-24587.11'))

# 10. Write a Python program to create Fibonacci series upto n using Lambda. 
# Fibonacci series upto 2:
# [0, 1]
# Fibonacci series upto 5:
# [0, 1, 1, 2, 3]
# Fibonacci series upto 6:
# [0, 1, 1, 2, 3, 5]
# Fibonacci series upto 9:
# [0, 1, 1, 2, 3, 5, 8, 13, 21]
from functools import reduce
#A bit complex workaround
fib_series = lambda n: reduce(lambda x, _: x+[x[-1]+x[-2]], range(n-2), [0, 1])

print("Fibonacci series upto 2:")
print(fib_series(2))
print("\nFibonacci series upto 5:")
print(fib_series(5))
print("\nFibonacci series upto 6:")
print(fib_series(6))
print("\nFibonacci series upto 9:")
print(fib_series(9))

# 11. Write a Python program to find intersection of two given arrays using Lambda. 
# Original arrays:
# [1, 2, 3, 5, 7, 8, 9, 10]
# [1, 2, 4, 8, 9]
# Intersection of the said arrays: [1, 2, 8, 9]
array_nums1 = [1, 2, 3, 5, 7, 8, 9, 10]
array_nums2 = [1, 2, 4, 8, 9]
print("Original arrays:")
print(array_nums1)
print(array_nums2)
result = list(filter(lambda x: x in array_nums1, array_nums2))
print ("\nIntersection of the said arrays: ",result)

# 12. Write a Python program to rearrange positive and negative numbers in a given array using Lambda. 
# Original arrays:
# [-1, 2, -3, 5, 7, 8, 9, -10]
# Rearrange positive and negative numbers of the said array:
# [2, 5, 7, 8, 9, -10, -3, -1]
array_nums = [-1, 2, -3, 5, 7, 8, 9, -10]
print("Original arrays:")
print(array_nums)
result = sorted(array_nums, key = lambda i: 0 if i == 0 else -1 / i)
print("\nRearrange positive and negative numbers of the said array:")
print(result)

# 13. Write a Python program to count the even, odd numbers in a given array of integers using Lambda. 
# Original arrays:
# [1, 2, 3, 5, 7, 8, 9, 10]
# Number of even numbers in the above array: 3
# Number of odd numbers in the above array: 5
array_nums = [1, 2, 3, 5, 7, 8, 9, 10]
print("Original arrays:")
print(array_nums)
odd_ctr = len(list(filter(lambda x: (x%2 != 0) , array_nums)))
even_ctr = len(list(filter(lambda x: (x%2 == 0) , array_nums)))
print("\nNumber of even numbers in the above array: ", even_ctr)
print("\nNumber of odd numbers in the above array: ", odd_ctr)

# 14. Write a Python program to find the values of length six in a given list using Lambda. 
# Sample Output:
# Monday
# Friday
# Sunday
weekdays = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
days = filter(lambda day: day if len(day)==6 else '', weekdays)
for d in days:
  print(d)

# 15. Write a Python program to add two given lists using map and lambda. 
# Original list:
# [1, 2, 3]
# [4, 5, 6]
# Result: after adding two list
# [5, 7, 9]
nums1 = [1, 2, 3]
nums2 = [4, 5, 6]
print("Original list:")
print(nums1)
print(nums2)
result = map(lambda x, y: x + y, nums1, nums2)
print("\nResult: after adding two list")
print(list(result))

# 16. Write a Python program to find the second lowest grade of any student(s) from the given names and grades of each student using lists and lambda. Input number of students, names and grades of each student. 
# Input number of students: 5
# Name: S ROY
# Grade: 1
# Name: B BOSE
# Grade: 3
# Name: N KAR
# Grade: 2
# Name: C DUTTA
# Grade: 1
# Name: G GHOSH
# Grade: 1
# Names and Grades of all students:
# [['S ROY', 1.0], ['B BOSE', 3.0], ['N KAR', 2.0], ['C DUTTA', 1.0], ['G GHOSH', 1.0]]
# Second lowest grade: 2.0
# Names:
# N KAR
students = []
sec_name = []
n = int(input("Input number of students: "))
for _ in range(n):
   s_name = input("Name: ")
   score = float(input("Grade: "))
   students.append([s_name,score])
print("\nNames and Grades of all students:")
print(students)
order =sorted(students, key = lambda x: int(x[1]))
second_low = next(
    (order[i][1] for i in range(n) if order[i][1] != order[0][1]), 0)
print("\nSecond lowest grade: ",second_low)
sec_student_name = [x[0] for x in order if x[1] == second_low]
sec_student_name.sort()
print("\nNames:")
for s_name in sec_student_name:
   print(s_name)

# 17. Write a Python program to find numbers divisible by nineteen or thirteen from a list of numbers using Lambda. 
# Orginal list:
# [19, 65, 57, 39, 152, 639, 121, 44, 90, 190]
# Numbers of the above list divisible by nineteen or thirteen:
# [19, 65, 57, 39, 152, 190]
nums = [19, 65, 57, 39, 152, 639, 121, 44, 90, 190]
print("Orginal list:")
print(nums)
result = list(filter(lambda x: (x % 19 == 0 or x % 13 == 0), nums))
print("\nNumbers of the above list divisible by nineteen or thirteen:")
print(result)

# 18. Write a Python program to find palindromes in a given list of strings using Lambda. 
# Orginal list of strings:
# ['php', 'w3r', 'Python', 'abcd', 'Java', 'aaa']
# List of palindromes:
# ['php', 'aaa']
texts = ["php", "w3r", "Python", "abcd", "Java", "aaa"]
print("Orginal list of strings:")
print(texts)
result = list(filter(lambda x: (x == "".join(reversed(x))), texts))
print("\nList of palindromes:")
print(result) 

# 19. Write a Python program to find all anagrams of a string in a given list of strings using lambda. 
# Orginal list of strings:
# ['bcda', 'abce', 'cbda', 'cbea', 'adcb']
# Anagrams of 'abcd' in the above string:
# ['bcda', 'cbda', 'adcb']
from collections import Counter
texts = ["bcda", "abce", "cbda", "cbea", "adcb"]
str = "abcd"
print("Orginal list of strings:")
print(texts)
result = list(filter(lambda x: (Counter(str) == Counter(x)), texts))
print("\nAnagrams of 'abcd' in the above string: ")
print(result)

# 20. Write a Python program to find the numbers of a given string and store them in a list, display the numbers which are bigger than the length of the list in sorted form. Use lambda function to solve the problem. 
# Original string: sdf 23 safs8 5 sdfsd8 sdfs 56 21sfs 20 5
# Numbers in sorted form:
# 20 23 56
str1 = "sdf 23 safs8 5 sdfsd8 sdfs 56 21sfs 20 5"
print("Original string: ",str1)
str_num = list(str1.split(' '))
lenght=len(str_num)
numbers=sorted([int(x) for x in str_num if x.isdigit()])
print('Numbers in sorted form:')
for i in ((filter(lambda x:x>lenght,numbers))):
    print(i,end=' ')

# 21. Write a Python program that multiply each number of given list with a given number using lambda function. Print the result. 
# Original list: [2, 4, 6, 9, 11]
# Given number: 2
# Result:
# 4 8 12 18 22
nums = [2, 4, 6, 9 , 11]
n = 2
print("Original list: ", nums)
print("Given number: ", n)
filtered_numbers=list(map(lambda number:number*n,nums))
print("Result:")
print(' '.join(map(str,filtered_numbers)))

# 22. Write a Python program that sum the length of the names of a given list of names after removing the names that starts with an lowercase letter. Use lambda function. 
# Result:
# 16
sample_names = ['sally', 'Dylan', 'rebecca', 'Diana', 'Joanne', 'keith']
sample_names=list(filter(lambda el:el[0].isupper() and el[1:].islower(),sample_names))
print("Result:")
print(len(''.join(sample_names)))

# 23. Write a Python program to calculate the sum of the positive and negative numbers of a given list of numbers using lambda function. 
# Original list: [2, 4, -6, -9, 11, -12, 14, -5, 17]
# Sum of the positive numbers: -32
# Sum of the negative numbers: 48
nums = [2, 4, -6, -9, 11, -12, 14, -5, 17]
print("Original list:",nums)

total_negative_nums = list(filter(lambda nums:nums<0,nums))
total_positive_nums = list(filter(lambda nums:nums>0,nums))

print("Sum of the positive numbers: ",sum(total_negative_nums))
print("Sum of the negative numbers: ",sum(total_positive_nums))

# 24. Write a Python program to find numbers within a given range where every number is divisible by every digit it contains. 
# Sample Output:
# [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22]
def divisible_by_digits(start_num, end_num):
    return [n for n in range(start_num, end_num+1) \
                if not any(map(lambda x: int(x) == 0 or n%int(x) != 0, str(n)))]
print(divisible_by_digits(1,22))

# 25. Write a Python program to create the next bigger number by rearranging the digits of a given number. 
# Original number: 12
# Next bigger number: 21
# Original number: 10
# Next bigger number: False
# Original number: 201
# Next bigger number: 210
# Original number: 102
# Next bigger number: 120
# Original number: 445
# Next bigger number: 454
def rearrange_bigger(n):
    #Break the number into digits and store in a list
    nums = list(str(n))
    for i in range(len(nums)-2,-1,-1):
        if nums[i] < nums[i+1]:
            z = nums[i:]
            y = min(filter(lambda x: x > z[0], z))
            z.remove(y)
            z.sort()
            nums[i:] = [y] + z
            return int("".join(nums))
    return False
n = 12
print("Original number:",n)
print("Next bigger number:",rearrange_bigger(n))
n = 10
print("\nOriginal number:",n)
print("Next bigger number:",rearrange_bigger(n))
n = 201
print("\nOriginal number:",n)
print("Next bigger number:",rearrange_bigger(n))
n = 102
print("\nOriginal number:",n)
print("Next bigger number:",rearrange_bigger(n))
n = 445
print("\nOriginal number:",n)
print("Next bigger number:",rearrange_bigger(n))

# 26. Write a Python program to find the list with maximum and minimum length using lambda. 
# Original list:
# [[0], [1, 3], [5, 7], [9, 11], [13, 15, 17]]
# List with maximum length of lists:
# (3, [13, 15, 17])
# List with minimum length of lists:
# (1, [0])
def max_length_list(input_list):
    max_length = max(len(x) for x in input_list )   
    max_list = max(input_list, key = lambda i: len(i))    
    return(max_length, max_list)
    
def min_length_list(input_list):
    min_length = min(len(x) for x in input_list )  
    min_list = min(input_list, key = lambda i: len(i))
    return(min_length, min_list)
      
list1 = [[0], [1, 3], [5, 7], [9, 11], [13, 15, 17]]
print("Original list:")
print(list1)
print("\nList with maximum length of lists:")
print(max_length_list(list1))
print("\nList with minimum length of lists:")
print(min_length_list(list1))

# 27. Write a Python program to sort each sublist of strings in a given list of lists using lambda. 
# Original list:
# [['green', 'orange'], ['black', 'white'], ['white', 'black', 'orange']]
# After sorting each sublist of the said list of lists:
# [['green', 'orange'], ['black', 'white'], ['black', 'orange', 'white']]
def sort_sublists(input_list):
 return [sorted(x, key = lambda x:x[0]) for x in input_list]
color1 = [["green", "orange"], ["black", "white"], ["white", "black", "orange"]]
print("\nOriginal list:")
print(color1)  
print("\nAfter sorting each sublist of the said list of lists:")
print(sort_sublists(color1))

# 28. Write a Python program to sort a given list of lists by length and value using lambda. 
# Original list:
# [[2], [0], [1, 3], [0, 7], [9, 11], [13, 15, 17]]
# Sort the list of lists by length and value:
# [[0], [2], [0, 7], [1, 3], [9, 11], [13, 15, 17]]
def sort_sublists(input_list):
 return sorted(input_list, key=lambda l: (len(l), l))
list1 = [[2], [0], [1, 3], [0, 7], [9, 11], [13, 15, 17]]
print("Original list:")
print(list1)
print("\nSort the list of lists by length and value:")
print(sort_sublists(list1))

# 29. Write a Python program to find the maximum value in a given heterogeneous list using lambda. 
# Original list:
# ['Python', 3, 2, 4, 5, 'version']
# Maximum values in the said list using lambda:
# 5
def max_val(list_val):
 return max(list_val, key = lambda i: (isinstance(i, int), i))

list_val = ['Python', 3, 2, 4, 5, 'version'] 
print("Original list:")
print(list_val)
print("\nMaximum values in the said list using lambda:")
print(max_val(list_val))

# 30. Write a Python program to sort a given matrix in ascending order according to the sum of its rows using lambda. 
# Original Matrix:
# [[1, 2, 3], [2, 4, 5], [1, 1, 1]]
# Sort the said matrix in ascending order according to the sum of its rows
# [[1, 1, 1], [1, 2, 3], [2, 4, 5]]
# Original Matrix:
# [[1, 2, 3], [-2, 4, -5], [1, -1, 1]]
# Sort the said matrix in ascending order according to the sum of its rows
# [[-2, 4, -5], [1, -1, 1], [1, 2, 3]]
def sort_matrix(M):
 return sorted(M, key=lambda matrix_row: sum(matrix_row))

matrix1 = [[1, 2, 3], [2, 4, 5], [1, 1, 1]]
matrix2 = [[1, 2, 3], [-2, 4, -5], [1, -1, 1]]

print("Original Matrix:")
print(matrix1)
print("\nSort the said matrix in ascending order according to the sum of its rows") 
print(sort_matrix(matrix1))
print("\nOriginal Matrix:")
print(matrix2) 
print("\nSort the said matrix in ascending order according to the sum of its rows") 
print(sort_matrix(matrix2))


# 31. Write a Python program to extract specified size of strings from a give list of string values using lambda. 
# Original list:
# ['Python', 'list', 'exercises', 'practice', 'solution']
# length of the string to extract:
# 8
# After extracting strings of specified length from the said list:
# ['practice', 'solution']
def extract_string(str_list1, l):
 return list(filter(lambda e: len(e) == l, str_list1))

str_list1 = ['Python', 'list', 'exercises', 'practice', 'solution'] 
print("Original list:")
print(str_list1)
l = 8
print("\nlength of the string to extract:")
print(l)
print("\nAfter extracting strings of specified length from the said list:")
print(extract_string(str_list1 , l))

# 32. Write a Python program to count float number in a given mixed list using lambda. 
# Original list:
# [1, 'abcd', 3.12, 1.2, 4, 'xyz', 5, 'pqr', 7, -5, -12.22]
# Number of floats in the said mixed list:
# 3
def count_integer(list1):
 ert = list(map(lambda i: isinstance(i, float), list1))
 return len([e for e in ert if e])
list1 = [1, 'abcd', 3.12, 1.2, 4, 'xyz', 5, 'pqr', 7, -5, -12.22]
print("Original list:")
print(list1)
print("\nNumber of floats in the said mixed list:")
print(count_integer(list1))

# 33. Write a Python program to check whether a given string contains a capital letter, a lower case letter, a number and a minimum length using lambda. 
# Input the string: W3resource
# ['Valid string.']
def check_string(str1):
    messg = [
    lambda str1: any(x.isupper() for x in str1) or 'String must have 1 upper case character.',
    lambda str1: any(x.islower() for x in str1) or 'String must have 1 lower case character.',
    lambda str1: any(x.isdigit() for x in str1) or 'String must have 1 number.',
    lambda str1: len(str1) >= 7                 or 'String length should be atleast 8.',]
    result = [x for x in [i(str1) for i in messg] if x != True]
    if not result:
        result.append('Valid string.')
    return result    
s = input("Input the string: ")
print(check_string(s))

# 34. Write a Python program to filter the height and width of students, which are stored in a dictionary using lambda. 
# Original Dictionary:
# {'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}
# Height> 6ft and Weight> 70kg:
# {'Cierra Vega': (6.2, 70)}
def filter_data(students):
 return dict(filter(lambda x: (x[1][0], x[1][1]) > (6.0, 70), students.items()))  
students = {'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}
print("Original Dictionary:")
print(students)
print("\nHeight> 6ft and Weight> 70kg:")
print(filter_data(students))

# 35. Write a Python program to check whether a specified list is sorted or not using lambda. 
# Original list:
# [1, 2, 4, 6, 8, 10, 12, 14, 16, 17]
# Is the said list is sorted!
# True
# Original list:
# [1, 2, 4, 6, 8, 10, 12, 14, 16, 17]
# Is the said list is sorted!
# False
def is_sort_list(nums, key=lambda x: x):
 return all(key(e) >= key(nums[i]) for i, e in enumerate(nums[1:]))
nums1 = [1,2,4,6,8,10,12,14,16,17]
print ("Original list:")
print(nums1)
print("\nIs the said list is sorted!")
print(is_sort_list(nums1)) 
nums2 = [2,3,8,4,7,9,8,2,6,5,1,6,1,2,3,4,6,9,1,2]
print ("\nOriginal list:")
print(nums1)
print("\nIs the said list is sorted!")
print(is_sort_list(nums2))

# 36. Write a Python program to extract the nth element from a given list of tuples using lambda. 
# Original list:
# [('Greyson Fulton', 98, 99), ('Brady Kent', 97, 96), ('Wyatt Knott', 91, 94), ('Beau Turnbull', 94, 98)]
# Extract nth element ( n = 0 ) from the said list of tuples:
# ['Greyson Fulton', 'Brady Kent', 'Wyatt Knott', 'Beau Turnbull']
# Extract nth element ( n = 2 ) from the said list of tuples:
# [99, 96, 94, 98]
def extract_nth_element(test_list, n):
 return list(map (lambda x:(x[n]), test_list))
students = [('Greyson Fulton', 98, 99), ('Brady Kent', 97, 96), ('Wyatt Knott', 91, 94), ('Beau Turnbull', 94, 98)] 
print ("Original list:")
print(students)
n = 0
print("\nExtract nth element ( n =",n,") from the said list of tuples:")
print(extract_nth_element(students, n))
n = 2
print("\nExtract nth element ( n =",n,") from the said list of tuples:")
print(extract_nth_element(students, n))

# 37. Write a Python program to sort a list of lists by a given index of the inner list using lambda. 
# Original list:
# [('Greyson Fulton', 98, 99), ('Brady Kent', 97, 96), ('Wyatt Knott', 91, 94), ('Beau Turnbull', 94, 98)]
# Sort the said list of lists by a given index ( Index = 0 ) of the inner list
# [('Beau Turnbull', 94, 98), ('Brady Kent', 97, 96), ('Greyson Fulton', 98, 99), ('Wyatt Knott', 91, 94)]
# Sort the said list of lists by a given index ( Index = 2 ) of the inner list
# [('Wyatt Knott', 91, 94), ('Brady Kent', 97, 96), ('Beau Turnbull', 94, 98), ('Greyson Fulton', 98, 99)]
def index_on_inner_list(list_data, index_no):
 return sorted(list_data, key=lambda x: x[index_no])
students = [('Greyson Fulton', 98, 99), ('Brady Kent', 97, 96), ('Wyatt Knott', 91, 94), ('Beau Turnbull', 94, 98)] 
print ("Original list:")
print(students)
index_no = 0
print("\nSort the said list of lists by a given index","( Index = ",index_no,") of the inner list")
print(index_on_inner_list(students, index_no))
index_no = 2
print("\nSort the said list of lists by a given index","( Index = ",index_no,") of the inner list")
print(index_on_inner_list(students, index_no))

# 38. Write a Python program to remove all elements from a given list present in another list using lambda. 
# Original lists:
# list1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# list2: [2, 4, 6, 8]
# Remove all elements from 'list1' present in 'list2:
# [1, 3, 5, 7, 9, 10]
def index_on_inner_list(list1, list2):
 return list(filter(lambda x: x not in list2, list1))
list1 = [1,2,3,4,5,6,7,8,9,10]
list2 = [2,4,6,8]
print("Original lists:")
print("list1:", list1)
print("list2:", list2)
print("\nRemove all elements from 'list1' present in 'list2:")
print(index_on_inner_list(list1, list2))


# 39. Write a Python program to find the elements of a given list of strings that contain specific substring using lambda. 
# Original list:
# ['red', 'black', 'white', 'green', 'orange']
# Substring to search:
# ack
# Elements of the said list that contain specific substring:
# ['black']
# Substring to search:
# abc
# Elements of the said list that contain specific substring:
# []
def find_substring(str1, sub_str):
 return list(filter(lambda x: sub_str in x, str1))
colors = ["red", "black", "white", "green", "orange"]
print("Original list:")
print(colors)

sub_str = "ack"
print("\nSubstring to search:")
print(sub_str)
print("Elements of the said list that contain specific substring:")
print(find_substring(colors, sub_str))
sub_str = "abc"
print("\nSubstring to search:")
print(sub_str)
print("Elements of the said list that contain specific substring:")
print(find_substring(colors, sub_str))

# 40. Write a Python program to find the nested lists elements, which are present in another list using lambda. 
# Original lists: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
# [[12, 18, 23, 25, 45], [7, 11, 19, 24, 28], [1, 5, 8, 18, 15, 16]]
# Intersection of said nested lists:
# [[12], [7, 11], [1, 5, 8]]
def intersection_nested_lists(l1, l2):
 return [list(filter(lambda x: x in l1, sublist)) for sublist in l2]
nums1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
nums2 = [[12, 18, 23, 25, 45], [7, 11, 19, 24, 28], [1, 5, 8, 18, 15, 16]]
print("\nOriginal lists:")
print(nums1)
print(nums2)
print("\nIntersection of said nested lists:")
print(intersection_nested_lists(nums1, nums2))

# 41. Write a Python program to reverse strings in a given list of string values using lambda. 
# Original lists:
# ['Red', 'Green', 'Blue', 'White', 'Black']
# Reverse strings of the said given list:
# ['deR', 'neerG', 'eulB', 'etihW', 'kcalB']
def reverse_strings_list(string_list):
 return list(map(lambda x: "".join(reversed(x)), string_list))

colors_list = ["Red", "Green", "Blue", "White", "Black"]
print("\nOriginal lists:")
print(colors_list)
print("\nReverse strings of the said given list:")
print(reverse_strings_list(colors_list))

# 42. Write a Python program to calculate the product of a given list of numbers using lambda. 
# list1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# Product of the said list numbers:
# 3628800
# list2: [2.2, 4.12, 6.6, 8.1, 8.3]
# Product of the said list numbers:
# 4021.8599520000007
import functools 
#NOTE: reduce function is a part of functools modules. DO NOT FORGET TO INCLUDE!
def remove_duplicates(nums):
 return functools.reduce(lambda x, y: x * y, nums, 1)
nums1 = [1,2,3,4,5,6,7,8,9,10]
nums2 = [2.2,4.12,6.6,8.1,8.3]
print("list1:", nums1)
print("Product of the said list numbers:")
print(remove_duplicates(nums1))
print("\nlist2:", nums2)
print("Product of the said list numbers:")
print(remove_duplicates(nums2))

# 43. Write a Python program to multiply all the numbers in a given list using lambda. 
'''SAME AS THE PROGRAM ABOVE'''


# 44. Write a Python program to calculate the average value of the numbers in a given tuple of tuples using lambda. 
# Original Tuple:
# ((10, 10, 10), (30, 45, 56), (81, 80, 39), (1, 2, 3))
# Average value of the numbers of the said tuple of tuples:
# (30.5, 34.25, 27.0)
# Original Tuple:
# ((1, 1, -5), (30, -15, 56), (81, -60, -39), (-10, 2, 3))
# Average value of the numbers of the said tuple of tuples:
# (25.5, -18.0, 3.75)
def average_tuple(nums):
 return tuple(map(lambda x: sum(x) / float(len(x)), zip(*nums)))

nums = ((10, 10, 10), (30, 45, 56), (81, 80, 39), (1, 2, 3))
print ("Original Tuple: ")
print(nums)
print("\nAverage value of the numbers of the said tuple of tuples:\n",average_tuple(nums))
nums = ((1, 1, -5), (30, -15, 56), (81, -60, -39), (-10, 2, 3))
print ("\nOriginal Tuple: ")
print(nums)
print("\nAverage value of the numbers of the said tuple of tuples:\n",average_tuple(nums))

# 45. Write a Python program to convert string element to integer inside a given tuple using lambda. 
# Original tuple values:
# (('233', 'ABCD', '33'), ('1416', 'EFGH', '55'), ('2345', 'WERT', '34'))
# New tuple values:
# ((233, 33), (1416, 55), (2345, 34))
def tuple_int_str(tuple_str):
 return tuple(map(lambda x: (int(x[0]), int(x[2])), tuple_str))     
tuple_str =  (('233', 'ABCD', '33'), ('1416', 'EFGH', '55'), ('2345', 'WERT', '34'))
print("Original tuple values:")
print(tuple_str)
print("\nNew tuple values:")
print(tuple_int_str(tuple_str))

# 46. Write a Python program to find index position and value of the maximum and minimum values in a given list of numbers using lambda. 
# Original list:
# [12, 33, 23, 10.11, 67, 89, 45, 66.7, 23, 12, 11, 10.25, 54]
# Index position and value of the maximum value of the said list:
# (5, 89)
# Index position and value of the minimum value of the said list:
# (3, 10.11)
def position_max_min(nums):
    max_result = max(enumerate(nums), key=(lambda x: x[1]))
    min_result = min(enumerate(nums), key=(lambda x: x[1]))
    return max_result,min_result

nums = [12,33,23,10.11,67,89,45,66.7,23,12,11,10.25,54]
print("Original list:")
print(nums)
result = position_max_min(nums)
print("\nIndex position and value of the maximum value of the said list:")
print(result[0])
print("\nIndex position and value of the minimum value of the said list:")
print(result[1])

# 47. Write a Python program to sort a given mixed list of integers and strings using lambda. Numbers must be sorted before strings. 
# Original list:
# [19, 'red', 12, 'green', 'blue', 10, 'white', 'green', 1]
# Sort the said mixed list of integers and strings:
# [1, 10, 12, 19, 'blue', 'green', 'green', 'red', 'white']
def sort_mixed_list(mixed_list):
    mixed_list.sort(key=lambda e: (isinstance(e, str), e))
    return mixed_list
mixed_list = [19,'red',12,'green','blue', 10,'white','green',1]
print("Original list:")
print(mixed_list)
print("\nSort the said  mixed list of integers and strings:")
print(sort_mixed_list(mixed_list))

# 48. Write a Python program to sort a given list of strings(numbers) numerically using lambda. 
# Original list:
# ['4', '12', '45', '7', '0', '100', '200', '-12', '-500']
# Sort the said list of strings(numbers) numerically:
# ['-500', '-12', '0', '4', '7', '12', '45', '100', '200']
def sort_numeric_strings(nums_str):
 return sorted(nums_str, key=int)
nums_str = ['4','12','45','7','0','100','200','-12','-500']
print("Original list:")
print(nums_str)
print("\nSort the said list of strings(numbers) numerically:")
print(sort_numeric_strings(nums_str))

# 49. Write a Python program to count the occurrences of the items in a given list using lambda. 
# Original list:
# [3, 4, 5, 8, 0, 3, 8, 5, 0, 3, 1, 5, 2, 3, 4, 2]
# Count the occurrences of the items in the said list:
# {3: 4, 4: 2, 5: 3, 8: 2, 0: 2, 1: 1, 2: 2}
def count_occurrences(nums):
 return dict(map(lambda el  : (el, list(nums).count(el)), nums))
nums = [3,4,5,8,0,3,8,5,0,3,1,5,2,3,4,2]
print("Original list:")
print(nums)
print("\nCount the occurrences of the items in the said list:")
print(count_occurrences(nums))

# 50. Write a Python program to remove specific words from a given list using lambda. 
# Original list:
# ['orange', 'red', 'green', 'blue', 'white', 'black']
# Remove words:
# ['orange', 'black']
# After removing the specified words from the said list:
# ['red', 'green', 'blue', 'white']
def remove_words(list1, remove_words):
 return list(filter(lambda word: word not in remove_words, list1))
        
colors = ['orange', 'red', 'green', 'blue', 'white', 'black']
remove_colors = ['orange','black']
print("Original list:")
print(colors)
print("\nRemove words:")
print(remove_colors)
print("\nAfter removing the specified words from the said list:")
print(remove_words(colors, remove_colors))

# 51. Write a Python program to find the maximum and minimum values in a given list of tuples using lambda function. 
# Original list with tuples:
# [('V', 62), ('VI', 68), ('VII', 72), ('VIII', 70), ('IX', 74), ('X', 65)]
# Maximum and minimum values of the said list of tuples:
# (74, 62)
def max_min_list_tuples(class_students):
    return_max = max(class_students,key=lambda item:item[1])[1]
    return_min = min(class_students,key=lambda item:item[1])[1]
    return return_max, return_min
    
class_students = [('V', 62), ('VI', 68), ('VII', 72), ('VIII', 70), ('IX', 74), ('X', 65)]
print("Original list with tuples:")
print(class_students)
print("\nMaximum and minimum values of the said list of tuples:")
print(max_min_list_tuples(class_students))

# 52. Write a Python program to remove None value from a given list using lambda function. 
# Original list:
# [12, 0, None, 23, None, -55, 234, 89, None, 0, 6, -12]
# Remove None value from the said list:
# [12, 0, 23, -55, 234, 89, 0, 6, -12]
def remove_none(nums):
    result = filter(lambda v: v is not None, nums)
    return list(result)

nums = [12, 0, None, 23, None, -55, 234, 89, None, 0, 6, -12]
print("Original list:")
print(nums)
print("\nRemove None value from the said list:")
print(remove_none(nums))






LinkedList.py # 1. Write a Python program to create a singly linked list, append some items and iterate through the list. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.head = None
        self.tail = None
        self.count = 0

    def iterate_item(self):
        # Iterate the list.
        current_item = self.head
        while current_item:
            val = current_item.data
            current_item = current_item.next
            yield val

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.tail:
            self.tail.next = node
        else:
            self.head = node
        self.tail = node
        self.count += 1


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
for val in items.iterate_item():
    print(val)
print("\nhead.data: ", items.head.data)
print("tail.data: ", items.tail.data)


# 2. Write a Python program to find the size of a singly linked list. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1

    def iterate_item(self):
        # Iterate the list.
        current_item = self.tail
        while current_item:
            val = current_item.data
            current_item = current_item.next
            yield val


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Original list:")
for val in items.iterate_item():
    print(val)

print("\nSize of the list:", items.count)


# 3. Write a Python program to search a specific item in a singly linked list and return true if the item is found otherwise return false. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1

    def iterate_item(self):
        # Iterate the list.
        current_item = self.tail
        while current_item:
            val = current_item.data
            current_item = current_item.next
            yield val

    def search_item(self, val):
        return any(val == node for node in self.iterate_item())


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

if items.search_item('SQL'):
    print("True")
else:
    print("False")

if items.search_item('C++'):
    print("True")
else:
    print("False")


# 4. Write a Python program to access a specific item in a singly linked list using index value. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None
class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0
	
    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1
    
    def __getitem__(self, index):
        if index > self.count - 1:
            return "Index out of range"
        current_val = self.tail
        for _ in range(index):
            current_val = current_val.next
        return current_val.data


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Search using index:")
print(items[0])
print(items[1])
print(items[4])
print(items[5])
print(items[10])


# 5. Write a Python program to set a new value of an item in a singly linked list using index value. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1

    def __getitem__(self, index):
        if index > self.count - 1:
            return "Index out of range"
        current_val = self.tail
        for _ in range(index):
            current_val = current_val.next
        return current_val.data

    def __setitem__(self, index, value):
        if index > self.count - 1:
            raise Exception("Index out of range.")
        current = self.tail
        for _ in range(index):
            current = current.next
        current.data = value


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Modify items by index:")
items[1] = "SQL"
print("New value: ", items[1])
items[4] = "Perl"
print("New value: ", items[4])


# 6. Write a Python program to delete the first item from a singly linked list. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1

    def delete_item(self, data):
        # Delete an item from the list
        current = self.tail
        prev = self.tail
        while current:
            if current.data == data:
                if current == self.tail:
                    self.tail = current.next
                else:
                    prev.next = current.next
                self.count -= 1
                return
            prev = current
            current = current.next

    def iterate_item(self):
        # Iterate the list.
        current_item = self.tail
        while current_item:
            val = current_item.data
            current_item = current_item.next
            yield val


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Original list:")
for val in items.iterate_item():
    print(val)

print("\nAfter removing the first item from the list:")
items.delete_item('PHP')
for val in items.iterate_item():
    print(val)


# 7. Write a Python program to delete the last item from a singly linked list. 
class Node:
    # Singly linked node
    def __init__(self, data=None):
        self.data = data
        self.next = None


class singly_linked_list:
    def __init__(self):
        # Createe an empty list
        self.tail = None
        self.head = None
        self.count = 0

    def append_item(self, data):
        #Append items on the list
        node = Node(data)
        if self.head:
            self.head.next = node
        else:
            self.tail = node
        self.head = node
        self.count += 1

    def delete_item(self, data):
        # Delete an item from the list
        current = self.tail
        prev = self.tail
        while current:
            if current.data == data:
                if current == self.tail:
                    self.tail = current.next
                else:
                    prev.next = current.next
                self.count -= 1
                return
            prev = current
            current = current.next

    def iterate_item(self):
        # Iterate the list.
        current_item = self.tail
        while current_item:
            val = current_item.data
            current_item = current_item.next
            yield val


items = singly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Original list:")
for val in items.iterate_item():
    print(val)

print("\nAfter removing the last item from the list:")
items.delete_item('Java')
for val in items.iterate_item():
    print(val)


# 8. Write a Python program to create a doubly linked list, append some items and iterate through the list(print forward). 
class Node(object):
    # Doubly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev


class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item

        self.count += 1

    def print_foward(self):
        for node in self.iter():
            print(node)

    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.data
            current = current.next
            yield item_val


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Items in the Doubly linked list: ")
items.print_foward()


# 9. Write a Python program to create a doubly linked list and print nodes from current position to first node. 
class Node(object):
    # Doubly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev


class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item

        self.count += 1

    def print_foward(self):
        for node in self.iter():
            print(node)

    def print_backward(self):
        current = self.tail
        while current:
            print(current.data)
            current = current.prev

    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.data
            current = current.next
            yield item_val


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')

print("Print Items in the Doubly linked backwards:")
items.print_backward()


# 10. Write a Python program to count the number of items of a given doubly linked list. 
class Node(object):
    # Singly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev

class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item 
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item

        self.count += 1


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
items.append_item('SQL')

print("Number of items of the  Doubly linked list:",items.count)


# 11. Write a Python program to print a given doubly linked list in reverse order. 
class Node(object):
    # Singly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev


class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item
        self.count += 1

    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.data
            current = current.next
            yield item_val

    def print_foward(self):
        for node in self.iter():
            print(node)

    def reverse(self):
        """ Reverse linked list. """
        current = self.head
        while current:
            temp = current.next
            current.next = current.prev
            current.prev = temp
            current = current.prev
        temp = self.head
        self.head = self.tail
        self.tail = temp


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
items.append_item('SQL')

print("Reverse list ")
items.reverse()
items.print_foward()


# 12. Write a Python program to insert an item in front of a given doubly linked list. 
class Node(object):
    # Singly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev


class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item
        self.count += 1

    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.data
            current = current.next
            yield item_val

    def print_foward(self):
        for node in self.iter():
            print(node)

    def insert_start(self, data):
        if self.head is not None:
            new_node = Node(data, None, None)
            new_node.next = self.head
            self.head.prev = new_node
            self.head = new_node
            self.count += 1


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
items.append_item('SQL')

print("Original list:")
items.print_foward()
print("\nAppend item in front of the list:")
items.insert_start("Perl")
items.print_foward()


# 13. Write a Python program to search a specific item in a given doubly linked list and return true if the item is found otherwise return false. 
class Node(object):
    # Singly linked node
    def __init__(self, data=None, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev


class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, data):
        # Append an item
        new_item = Node(data, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item
        self.count += 1

    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.data
            current = current.next
            yield item_val

    def print_foward(self):
        for node in self.iter():
            print(node)

    def search_item(self, val):
        return any(val == node for node in self.iter())


items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
items.append_item('SQL')

print("Original list:")
items.print_foward()
print("\n")
if items.search_item('SQL'):
    print("True")
else:
    print("False")

if items.search_item('C+'):
    print("True")
else:
    print("False")


# 14. Write a Python program to delete a specific item from a given doubly linked list. 
class Node(object):
    # Singly linked node
    def __init__(self, value=None, next=None, prev=None):
        self.value = value
        self.next = next
        self.prev = prev

class doubly_linked_list(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.count = 0

    def append_item(self, value):
        # Append an item 
        new_item = Node(value, None, None)
        if self.head is None:
            self.head = new_item
            self.tail = self.head
        else:
            new_item.prev = self.tail
            self.tail.next = new_item
            self.tail = new_item
        self.count += 1
    
    def iter(self):
        # Iterate the list
        current = self.head
        while current:
            item_val = current.value
            current = current.next
            yield item_val

    def print_foward(self):
        for node in self.iter():
            print(node)   
        
    def search_item(self, val):
        return any(val == node for node in self.iter())
     
    def delete(self, value):
        # Delete a specific item
        current = self.head
        node_deleted = False
        if current is None:
            node_deleted = False

        elif current.value == value:
            self.head = current.next
            self.head.prev = None
            node_deleted = True

        elif self.tail.value == value:
            self.tail = self.tail.prev
            self.tail.next = None
            node_deleted = True

        else:
            while current:
                if current.value == value:
                    current.prev.next = current.next
                    current.next.prev = current.prev
                    node_deleted = True
                current = current.next

        if node_deleted:
            self.count -= 1

items = doubly_linked_list()
items.append_item('PHP')
items.append_item('Python')
items.append_item('C#')
items.append_item('C++')
items.append_item('Java')
items.append_item('SQL')

print("Original list:")
items.print_foward()

items.delete("Java")
items.delete("Python")
print("\nList after deleting two items:")
items.print_foward()





List.py """
1. Write a Python program to sum all the items in a list.
"""


my_list = [1, 2, 3, 4, 5]

list_sum = sum(my_list)
print(list_sum)

# solution 2
list_sum = sum(my_list)
print(list_sum)

"""
2. Write a Python program to multiply all the items in a list.
"""

my_list = [1, 2, 3, 4, 5]

list_prod = 1
for i in my_list:
    list_prod *= i
print(list_prod)

"""
3. Write a Python program to get the largest number from a list.
"""

my_list = [1, 2, 3, 4, 5]

# solution 1
max_num = 0
for i in my_list:
    if i > max_num:
        max_num = i
print(max_num)

# solution 2
max_num = max(my_list)
print(max_num)

"""
4. Write a Python program to get the smallest number from a list.
"""

my_list = [1, 2, 3, 4, 5]

# solution 1
sorted_list = sorted(my_list)
min_num = sorted_list[0]
print(min_num)

# solution 2
min_num = min(my_list)
print(min_num)

# solution 3
min_num = my_list[0]
for i in my_list:
    if i < min_num:
        min_num = i
print(min_num)

"""
5. Write a Python program to count the number of strings where
the string length is 2 or more and the first and last character
are same from a given list of strings. 
Sample List : ['abc', 'xyz', 'aba', '1221']
Expected Result : 2
"""

my_list = ['abc', 'xyz', 'aba', '1221']

counter = sum(len(i) > 1 and i[0] == i[len(i)-1] for i in my_list)
print(counter)

"""
6. Write a Python program to get a list, sorted in increasing order
by the last element in each tuple from a given list of non-empty tuples.
Sample List : [(2, 5), (1, 2), (4, 4), (2, 3), (2, 1)]
Expected Result : [(2, 1), (1, 2), (2, 3), (4, 4), (2, 5)]
"""

# solution 1
my_list = [(2, 5), (1, 2), (4, 4), (2, 3), (2, 1)]
sorted_list = sorted(my_list, key=lambda x: x[1])
print(sorted_list)

# solution 2
my_list = [(2, 5), (1, 2), (4, 4), (2, 3), (2, 1)]
my_list.sort(key=lambda x: x[1])
print(my_list)

"""
7. Write a Python program to remove duplicates from a list.
"""

my_list = [1, 2, 3, 4, 4, 5, 1, 2, 3, 4, 4, 5, 1, 1]

# solution 1
new_list = list(set(my_list))
print(new_list)

# solution 2
new_list = []
for i in my_list:
    if i not in new_list:
       new_list.append(i)
print(new_list)

"""
8. Write a Python program to check a list is empty or not
"""

my_list = []

if my_list:
    print('list is not empty')
else:
    print('list is empty')

"""
10. Write a Python program to find the list of words that are longer
than n from a given list of words.
"""

def is_longer(words, n):
    return [word for word in words if len(word) > n]


my_list = ['cat', 'dog', 'apple', 'table', 'bags']

print(is_longer(my_list, 3))

"""
11. Write a Python function that takes two lists and returns True
if they have at least one common member.
"""

list_one = [1, 2, 3, 4, 5]
list_two = [8, 9, 5, 10, 11]

def common_member(li1, li2):
    return any(i in li2 for i in li1)

print(common_member(list_one, list_two))

"""
12. Write a Python program to print a specified list after removing the 0th,
4th and 5th elements.
Sample List : ['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']
Expected Output : ['Green', 'White', 'Black']
"""

my_list = ['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']

new_list = [
    value for index, value in enumerate(my_list) if index not in [0, 4, 5]
]
print(new_list)

# solution 2
new_list = [x for i, x in enumerate(my_list) if i not in (0,4,5)]
print(new_list)

"""
13. Write a Python program to generate a 3*4*6 3D array whose each element is *.
"""

my_array = [[['*' for _ in range(6)] for _ in range(4)] for _ in range(3)]
print(my_array)

"""
14. Write a Python program to print the numbers of a specified list
after removing even numbers from it
"""

my_list = [1, 2, 3, 4, 5]

new_list = [x for x in my_list if x%2 != 0]
print(new_list)

"""
15. Write a Python program to shuffle and print a specified list.
"""

from random import shuffle

my_list = [1, 2, 3, 4, 5]
shuffle(my_list)
print(my_list)

"""
16. Write a Python program to generate and print a list of first
and last 5 elements where the values are square of numbers between
1 and 30 (both included).
"""

my_list = [x**2 for x in range(1,31)]
print(my_list[:5],my_list[-5:])

"""
17. Write a Python program to generate and print a list except for the first
5 elements, where the values are square of numbers between 1 and 30 (both included).
"""
my_list = [x**2 for x in range(1,31)]
print(my_list[5:])

"""
18. Write a Python program to generate all permutations of a list in Python.
"""

import itertools

my_list = [1, 2, 3, 4, 5]
print(list(itertools.permutations(my_list)))

"""
19. Write a Python program to get the difference between the two lists.
"""

list_a = [1, 2, 3, 4, 5, 6, 'yellow']
list_b = [1, 2, 3, 4, 5]
diff_list = list(set(list_a) - set(list_b))
print(diff_list)

"""
20. Write a Python program access the index of a list.
"""

my_list = [1, 2, 3, 4, 5]

for index, value in enumerate(my_list):
    print(index)

"""
21. Write a Python program to convert a list of characters into a string.
"""

my_list = ['1', '2', '3', '4', '5']
my_string = ''.join(my_list)
print(my_string)

"""
22. Write a Python program to find the index of an item in a specified list.
"""

my_list = [1, 2, 3, 4, 5]

my_index = my_list.index(3)
print(my_index)

"""
23. Write a Python program to flatten a shallow list.
"""

my_list = [[1, 2], [3, 4], [5]]

# solution 1
new_list = []
for i in my_list:
    new_list.extend(iter(i))
print(new_list)

# solution 2
from itertools import chain

new_list = list(chain.from_iterable(my_list))
print(new_list)

# solution 3
from itertools import chain

new_list = list(chain(*my_list))
print(new_list)

"""
24. Write a Python program to append a list to the second list.
"""

list_a = [1, 2, 3, 4, 5, 6, 'yellow']
list_b = [1, 2, 3, 4, 5]

combi_list = list_a + list_b
print(combi_list)

"""
25. Write a Python program to select an item randomly from a list.
"""

import random

my_list = [1, 2, 3, 4, 5]
rand_element = random.choice(my_list)
print(rand_element)

"""
26. Write a python program to check whether two lists are circularly identical.
"""

list_a = [1, 2, 3, 4, 5]
list_b = [5, 1, 2, 3, 4]
def circularly_identical(list_a, list_b):
    if len(list_a) != len(list_b):
        return print('not circularly identical')  
    double_a = list_a * 2   # double_a = list_a + list_a
    string_b = map(str, list_b)
    string_double_a = map(str, double_a)
    if ' '.join(string_b) in ' '.join(string_double_a):
        return print('circularly identical')
    else:
        return print('not circularly identical')

circularly_identical(list_a, list_b)

"""
27. Write a Python program to find the second smallest number in a list.
"""

my_list = [1, 2, 3, 4, 5]

sorted_list = sorted(my_list)
print(sorted_list[1])

"""
28. Write a Python program to find the second largest number in a list.
"""
my_list = [1, 2, 3, 4, 5]

sorted_list = sorted(my_list)
print(sorted_list[-2])

"""
29. Write a Python program to get unique values from a list.
"""

my_list = [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]

print(set(my_list))

"""
30. Write a Python program to get the frequency of the elements in a list.
"""

my_list = [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 5, 5, 18, 'yellow']

freq_list = [(i, my_list.count(i)) for i in set(my_list)]
print(freq_list)

# soution 2
my_dict = dict(map(lambda x: (x, 0), my_list))
for i in my_dict:
    my_dict[i] = my_list.count(i)
print(my_dict)

"""
31. Write a Python program to count the number of elements in a list
within a specified range.
"""

def count_elements_in_range(my_list, min_range, max_range):
    return sum(element in range(min_range, max_range+1) for element in my_list)

my_list = [1, 2, 3, 4, 5]
print(count_elements_in_range(my_list, 3, 10))

"""
32. Write a Python program to check whether a list contains a sublist.
"""

def is_sublist(main_list, sub_list):
    return ''.join(map(str,sub_list)) in ''.join(map(str,main_list))

my_list = [1, 2, 3, 4, 5]
my_sublist_one = [2, 3]
my_sublist_two = [3, 2]

print('the list', my_list, 'contains sublist', my_sublist_one, '-',
      is_sublist(my_list, my_sublist_one))
print('the list', my_list, 'contains sublist', my_sublist_two, '-',
      is_sublist(my_list, my_sublist_two))

"""
33. Write a Python program to generate all sublists of a list.
"""
import itertools

my_list = [1, 2, 3, 4, 5]

# solution 1
def sublist_generator(my_list):
    for i in range(len(my_list)):
        for j in range(len(my_list),i,-1):
            yield list(itertools.islice(my_list, i, j))

print(list(sublist_generator(my_list)))

# solution 2
def sublist_generator(my_list):
    sublists = []
    for i in range(len(my_list)):
        j = len(my_list)
        while j > i:
            sublists.append(my_list[i:j])
            j -= 1
    return sublists
print(sublist_generator(my_list))

"""
34. Write a Python program using Sieve of Eratosthenes method for computing
primes upto a specified number. 
Note: In mathematics, the sieve of Eratosthenes, one of a number of prime
number sieves, is a simple, ancient algorithm for finding all prime numbers
up to any given limit.
"""

# solution 1

n = int(input('please enter a number ')) + 1
not_primes = []
primes = []
for p in range(2, n):
    if p not in not_primes:
        primes.append(p)
        not_primes.extend(iter(range(p**2, n, p)))
print(primes)

# solution 2 
def update_list(p, num_list):
    return [x for x in num_list if x%p != 0]

n = int(input('please enter a number ')) + 1
primes = []
num_list = list(range(2, n))
while num_list:
    p = num_list[0]
    primes.append(p)
    num_list = update_list(p, num_list)
print(primes)

"""
35. Write a Python program to create a list by concatenating a given list
which range goes from 1 to n. 
Sample list : ['p', 'q']
n =5
Sample Output : ['p1', 'q1', 'p2', 'q2', 'p3', 'q3', 'p4', 'q4', 'p5', 'q5']
"""

my_list = ['p', 'q']
n = 5

# solution 1
new_list = []
for i in range(1, n+1):
    for element in my_list:
        new_list.append(str(element)+str(i))
print(new_list)

# solution 2
new_list = ['{}{}'.format(x, y) for y in range(1, n+1) for x in my_list]
print(new_list)

"""
37. Write a Python program to find common items from two lists.
"""

list_one = [1, 2, 3, 4, 5, 'red']
list_two = ['yellow', 'red', 'blue', 2, 5]

# solution 1
common_items = []
for i in list_one:
    if i in list_two:
        common_items.append(i)
print('list one is', list_one, ', list two is', list_two,
      'their common items are', common_items)

# solution 2
common_items = list(set(list_one) & set(list_two))     # common_items = list(set(list_one).intersection(set(list_two)))
print('list one is', list_one, ', list two is', list_two,
      'their common items are', common_items)

"""
38. Write a Python program to change the position of every n-th value with
the (n+1)th in a list. 
Sample list: [0,1,2,3,4,5]
Expected Output: [1, 0, 3, 2, 5, 4]
"""

my_list = [0, 1, 2, 3, 4, 5]
n = 2
new_list = []
for i in range(0, len(my_list), n):
    new_list.append(my_list[i+1])
    new_list.append(my_list[i])

print(new_list)

"""
39. Write a Python program to convert a list of multiple integers into
a single integer.
"""

my_list = [0, 1, 2, 3, 4, 5]

if my_list[0] == 0:
    my_list.pop(0)

single_int = int(''.join(map(str,my_list)))
print('from list', my_list, 'to single integer', single_int)

"""
40. Write a Python program to split a list based on first character of word.
"""
import string 
text = input('please enter some text \n')
text_list = text.lower().split()
word_dict = {}
for word in text_list:
    for letter in string.ascii_lowercase:
        if word.startswith(letter):
            if letter in word_dict:
                word_dict[letter].append(word)
            else:
                word_dict[letter] = [word]
print(word_dict)

"""
42. Write a Python program to find missing and additional values in two lists.
Sample data : Missing values in second list: b,a,c
Additional values in second list: g,h
"""

list_one = [1, 2, 3, 4, 'x', 'a', 'b', 'c']
list_two = [1, 2, 3, 'x', 'g', 'h']

print('missing', set(list_one) - set(list_two))
print('additional', set(list_two) - set(list_one))

"""
43. Write a Python program to split a list into different variables.
"""

my_list = [1, 2, 'a']

x, y, z = my_list

print(x)
print(y)
print(z)

"""
44. Write a Python program to generate groups of five consecutive numbers
in a list (list of consecutive numbers grouped in lists of 5).
"""

my_list = [[(5*x + y) for y in range(1,6)] for x in range(10)]
print(my_list)

"""
45. Write a Python program to convert a pair of values into a sorted unique array.
(list of tuples to sorted list of values).
"""

my_list = [(7, 0), (1, 2), (7, 8), (4, 3), (6, 9), (3, 4)]

# solution 1
new_list = []
for x in my_list:
    for y in x:
        new_list.append(y)
new_set = set(new_list)
print(sorted(new_set))

# solution 2 (same as solution 1 only with list comprehension)
new_list = [y for x in my_list for y in x]
new_set = set(new_list)
print(sorted(new_set))

# solution 3
from itertools import chain

new_list = chain(*my_list)
new_set = set(new_list)
print(sorted(new_set))

# solution 4
new_set = set()
new_set = new_set.union(*my_list)
print(sorted(new_set))

"""
46. Write a Python program to select the odd items of a list.
"""

my_list = [0, 1, 2, 3, 4, 5, 6, 7, 8]

new_list = [x for x in my_list if x%2 != 0]
print(new_list)

"""
47. Write a Python program to insert an element before each element of a list.
"""

# solution 1
my_list = [0, 1, 2, 3, 4, 5]
element = 'x'
for i in range(0,len(my_list)*2,2):
    my_list.insert(i, element)
print(my_list)

# solution 2
my_list = [0, 1, 2, 3, 4, 5]
element = 'x'
new_list = [y for x in my_list for y in (element, x)]
print(new_list)

"""
48. Write a Python program to print a nested lists (each list on a new line)
using the print() function.
"""

my_list = [ [7, 0],  [1, 2],  [7, 8],  [4, 3],  [6, 9],  [3, 4]]

for i in my_list:
    print(i)
    
"""
49. Write a Python program to convert list to list of dictionaries.
Sample lists:
["Black", "Red", "Maroon", "Yellow"],
["#000000", "#FF0000", "#800000", "#FFFF00"]
Expected Output:
[{'color_name': 'Black', 'color_code': '#000000'},
{'color_name': 'Red', 'color_code': '#FF0000'},
{'color_name': 'Maroon', 'color_code': '#800000'},
{'color_name': 'Yellow', 'color_code': '#FFFF00'}]
"""

names = ["Black", "Red", "Maroon", "Yellow"]
codes = ["#000000", "#FF0000", "#800000", "#FFFF00"]

colors_list = [dict(color_name=x, color_code=y) for (x,y) in zip(names, codes)]
print(colors_list)

"""
51. Write a Python program to split a list every Nth element.
Sample list:
['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']
Expected Output:
[['a', 'd', 'g', 'j', 'm'], ['b', 'e', 'h', 'k', 'n'], ['c', 'f', 'i', 'l']]
"""

my_list = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']
N = 3
big_list = []
big_list = [[my_list[i] for i in range(n, len(my_list), N)] for n in range(N)]
print(big_list)

"""
52. Write a Python program to compute the similarity between two lists.
Sample data:
["red", "orange", "green", "blue", "white"],
["black", "yellow", "green", "blue"]
Expected Output: 
Color1-Color2: ['white', 'orange', 'red']
Color2-Color1: ['black', 'yellow']
"""

list_one = ["red", "orange", "green", "blue", "white"]
list_two = ["black", "yellow", "green", "blue"]

print('Color1-Color2', list(set(list_one) - set(list_two)))
print('Color2-Color1', list(set(list_two) - set(list_one)))

"""
54. Write a Python program to concatenate elements of a list. 
"""

my_list = [1, 2, 3, 4, 5, 'a']

my_string = ''.join(map(str, my_list))
print(my_string)

"""
55. Write a Python program to remove key values pairs from a list of dictionaries.
"""

# solution 1
my_list = [{'x': 1, 'y': 2},
           {'x': 3, 'y': 4},
           {'x': 5, 'y': 6}]

for i in my_list:
    del(i['x'])
print(my_list)

# solution 2
my_list = [{'x': 1, 'y': 2},
           {'x': 3, 'y': 4},
           {'x': 5, 'y': 6}]

new_list = [{k: v for k, v in x.items() if k != 'x'} for x in my_list]
print(new_list)

"""
56. Write a Python program to convert a string to a list.
"""

my_string = 'abracadabra'

# list of letters
my_list = list(my_string)
print(my_list)
# list of words
my_list = my_string.split()
print(my_list)

# convert a "string" list example color ="['Red', 'Green', 'White']" to real list
my_string = "['Red', 'Green', 'White']"
print(my_string)
my_string = my_string.strip('[]')
my_string = my_string.replace(',', ' ')
my_string = my_string.replace("'", ' ')
my_list = my_string.split()
print(my_list)

"""
57. Write a Python program to check if all items of a list is equal
to a given string.
"""

my_list = ['a', 'a', 'a', 'a']
control_list = ['a', 'a', 'a', 'b']
my_string = 'a'

# long solution
def items_equal(my_list, my_string):
    counter = 0
    for i in my_list:
        if i == my_string:
            counter += 1
    if counter == len(my_list):
        return True
    else:
        return False

if items_equal(my_list, my_string):
    print('all items in list', my_list, 'are equal to', my_string)
else:
    print('NOT all items in list', my_list, 'are equal to', my_string)   

if items_equal(control_list, my_string):
    print('all items in list', control_list, 'are equal to', my_string)
else:
    print('NOT all items in list', control_list, 'are equal to', my_string) 

# short solution
if all(i == my_string for i in my_list):
    print('all items in list', my_list, 'are equal to', my_string)
else:
    print('NOT all items in list', my_list, 'are equal to', my_string)  

if all(i == my_string for i in control_list):
    print('all items in list', control_list, 'are equal to', my_string)
else:
    print('NOT all items in list', control_list, 'are equal to', my_string) 

"""
58. Write a Python program to replace the last element in a list with
another list. 
Sample data : [1, 3, 5, 7, 9, 10], [2, 4, 6, 8]
Expected Output: [1, 3, 5, 7, 9, 2, 4, 6, 8]
"""

list_one = [1, 3, 5, 7, 9, 10]
list_two = [2, 4, 6, 8]
list_one[len(list_one)-1:] = list_two  # list_one[-1:] = list_two
print(list_one)

"""
59. Write a Python program to check if the n-th element exists in a given list.
"""

my_list = [1, 3, 5, 7, 9, 10]
n = 6

# solution 1
if len(my_list) > n:
    print("there is {}-th element in list".format(n), my_list)
else:
    print("no {}-th element in list".format(n), my_list)
    

# solution 2
try:
    my_list[n]
    print("there is {}-th element in list".format(n), my_list)    
except:
    print("no {}-th element in list".format(n), my_list)

"""
60. Write a Python program to find a tuple with the smallest 'second
index' value from a list of tuples.
"""

my_list = [(1, 2), (9, 6), (3, 5), (3, 1), (6, 3)]
print(my_list)

# solution 1
my_list.sort(key=lambda x: x[1])
print('tuple with the smallest second value is', my_list[0])

# solution 2
my_list = [(1, 2), (9, 6), (3, 5), (3, 1), (6, 3)]
print('tuple with the smallest second value is', min(my_list, key=lambda x: x[1]))

"""
61. Write a Python program to create a list of empty dictionaries.
"""

my_list = [{} for i in range(5)]
print(my_list)

"""
62. Write a Python program to print a list of space-separated elements.
"""

my_list = [1, 2, 3, 4, 5]

# solution 1
print(' '.join(map(str, my_list)))

# solution 2
print(*my_list)

"""
63. Write a Python program to insert a given string at the beginning of all
items in a list.
Sample list : [1,2,3,4], string : emp
Expected output : ['emp1', 'emp2', 'emp3', 'emp4']
"""

my_list = [1,2,3,4]
my_string = 'emp'

# solution 1
new_list = [my_string+str(x) for x in my_list]
print(new_list)

"""
64. Write a Python program to iterate over two lists simultaneously.
"""

list_one = [1, 2, 3, 4, 5]
list_two = [11, 22, 33, 44, 55]

for x, y in zip(list_one, list_two):
    my_tuple = (x, y)
    print(my_tuple)

"""
65. Write a Python program to access dictionary keys element by index.
"""

my_list = {'john': 15, 'dave': 12, 'jack': 10}
list_of_keys = list(my_list)
for i in range(len(list_of_keys)):
    print('the key', list_of_keys[i], 'has index', i)

"""
66. Write a Python program to find the list in a list of lists whose
sum of elements is the highest. 
Sample lists: [1,2,3], [4,5,6], [10,11,12], [7,8,9]
Expected Output: [10, 11, 12]
"""

my_list = [[1,2,3], [4,5,6], [10,11,12], [7,8,9]]

print(max(my_list, key=lambda x: sum(x)))  # print(max(my_list, key=sum))

"""
67. Write a Python program to find all the values in a list that are greater
than a specified number.
"""

my_list = [1, 2, 3, 4, 5]
num = 3
new_list = [x for x in my_list if x > num]
print(new_list)

"""
67. Write a Python program to find if all the values in a list are greater
than a specified number.
"""

my_list = [1, 2, 3, 4, 5]
num = 3
print(all(x > num for x in my_list))
num = 0
print(all(x > num for x in my_list))

"""
68. Write a Python program to extend a list without append.
Sample data: [10, 20, 30] [40, 50, 60]
Expected output : [40, 50, 60, 10, 20, 30]
"""

# solution 1
list_one = [10, 20, 30]
list_two = [40, 50, 60]
list_two.extend(list_one)
print(list_two)

# solution 2
list_one = [10, 20, 30]
list_two = [40, 50, 60]
list_two[len(list_two):] = list_one
print(list_two)

"""
69. Write a Python program to remove duplicates from a list of lists.
Sample list : [[10, 20], [40], [30, 56, 25], [10, 20], [33], [40]]
New List : [[10, 20], [30, 56, 25], [33], [40]]
"""

my_list = [[10, 20], [40], [30, 56, 25], [10, 20], [33], [40]]
print(my_list)

sorted_list = sorted(my_list)
unique_list = []
for i in my_list:
    if i in unique_list:
        continue
    else:
        unique_list.append(i)
print(unique_list)

"""
70. Write a Python program to get the depth of a dictionary.
"""


my_dict = {'a1': 2, 'a2': {'b': {'c': {'x': {'z': 1}}}}}

def dict_depth(my_dict):
    depth = 0
    if isinstance(my_dict, dict):
        depth += 1
        for key in my_dict:
            depth += dict_depth(my_dict[key])
    return depth
print(dict_depth(my_dict))


"""
71. Write a Python program to check if all dictionaries in a list
are empty or not. 
Sample list : [{},{},{}]
Return value : True
Sample list : [{1,2},{},{}]
Return value : False
"""

# solution 1
def is_empty(my_list):
    for i in my_list:
        if i:
            return False
    return True

print(is_empty([{},{},{}]))
print(is_empty([{1,2},{},{}]))

# solution 2
print(all(not x for x in [{},{},{}]))
print(all(not x for x in [{1,2},{},{}]))




LoopsAndConditionalStatements.py """
1. Write a Python program to find those numbers which are divisible by 7
and multiple of 5, between 1500 and 2700 (both included).
"""


my_list = [i for i in range(1500, 2701) if i%7 == 0 and i%5 == 0]
print(my_list)

"""
2. Write a Python program to convert temperatures to and from celsius, fahrenheit.
Formula : c/5 = (f-32)/9 [ where c=temp in celsius and f=temp in fahrenheit 
Expected Output : 
60°C is 140 in Fahrenheit
45°F is 7 in Celsius
"""

def celsius_fahrenheit_conversion(temperature, scale_name='F'):
    if scale_name == 'C':
        converted = temperature*9/5+32
    elif scale_name == 'F':
        converted = (temperature-32)*5/9
    return converted

t = float(input('enter temperature '))
s = input('enter F for Fahrenheit or C for Celsius ').upper()

if s == 'F':
    print('%.0f°F is %.0f in Celsius' % (t, celsius_fahrenheit_conversion(t, s)))
elif s == 'C':
    print('%.0f°C is %.0f in Fahrenheit' % (t, celsius_fahrenheit_conversion(t, s)))
    
"""
3. Write a Python program to guess a number between 1 to 9. 
Note : User is prompted to enter a guess. If the user guesses wrong then
the prompt appears again until the guess is correct, on successful guess,
user will get a "Well guessed!" message, and the program will exit.
"""

from random import randint

number = randint(1, 9)
guess = int(input('guess a number between 1 and 9: '))

while guess != number:
    guess = int(input('you guessed wrong, guess again: '))

print('Well guesed!')

"""
4. Write a Python program to construct the following pattern,
using a nested for loop.
* 
* * 
* * * 
* * * * 
* * * * * 
* * * * 
* * * 
* * 
*
"""

width = int(input('choose pattern width '))

# solution with 2 for loops
for i in range(1, width):
    print('*' * i)
for i in range(width, 0, -1):
    print('*' * i)

# solution with for loop and if statement
for row in range(1, width*2):
    if row < width:
        print('*' * row)
    else:
        print('*' * (width*2 - row))

"""
5. Write a Python program that accepts a word from the user and reverse it.
"""

word = input('please eneter a word ')

# solution 1
print(''.join((reversed(word))))

# solution 2
word_list = list(word)
print(''.join(word_list[::-1]))

# solution 3
reversed_word = ''
for i in range(len(word)-1, -1, -1):
    reversed_word += word[i]
print(reversed_word)

"""
6. Write a Python program to count the number of even and odd numbers
from a series of numbers. 
Sample numbers : numbers = (1, 2, 3, 4, 5, 6, 7, 8, 9) 
Expected Output : 
Number of even numbers : 5
Number of odd numbers : 4
"""
    
numbers = (1, 2, 3, 4, 5, 6, 7, 8, 9)
odds_counter = 0
evens_counter = 0
for i in numbers:
    if i%2 == 0:
        evens_counter += 1
    else:
        odds_counter += 1

print('Number of even numbers: %d' % evens_counter)
print('Number of odd numbers: %d' % odds_counter)

"""
7. Write a Python program that prints each item and its corresponding type
from the following list.
Sample List : datalist = [1452, 11.23, 1+2j, True, 'w3resource',
(0, -1), [5, 12], {"class":'V', "section":'A'}]
"""
datalist = [1452, 11.23, 1+2j, True, 'w3resource', (0, -1),
            [5, 12], {"class":'V', "section":'A'}]

for i in datalist:
    print(i, type(i))

"""
8. Write a Python program that prints all the numbers from 0 to 6 except 3 and 6.
Note : Use 'continue' statement. 
Expected Output : 0 1 2 4 5
"""

for i in range(7):
    if i == 3 or i == 6:
        continue
    print(i, end = ' ')

"""
9. Write a Python program to get the Fibonacci series between 0 to 50.
Note : The Fibonacci Sequence is the series of numbers :
0, 1, 1, 2, 3, 5, 8, 13, 21, .... 
Every next number is found by adding up the two numbers before it.
Expected Output : 1 1 2 3 5 8 13 21 34
"""
fib_list = [1, 1]
while fib_list[-1] <= 50:
    fib_list.append(fib_list[-1]+fib_list[-2])
print(fib_list[:-1])

"""
10. Write a Python program which iterates the integers from 1 to 50. For multiples of three print "Fizz" instead of the number and for the multiples of five print "Buzz". For numbers which are multiples of both three and five print "FizzBuzz".
Sample Output : 
1
2
fizz
4 
buzz
"""
for i in range(1, 51):
    if i%3 == 0 and i%5 == 0:
        print('fizzbuzz')
    elif i%3 == 0:
        print('fizz')
    elif i%5 == 0:
        print('buzz')
    else:
        print(i)

"""
11. Write a Python program which takes two digits m (row) and n (column)
as input and generates a two-dimensional array. The element value in the i-th
row and j-th column of the array should be i*j. 
Note :
i = 0,1.., m-1 
j = 0,1, n-1.
Test Data : Rows = 3, Columns = 4 
Expected Result : [[0, 0, 0, 0], [0, 1, 2, 3], [0, 2, 4, 6]]
"""

Rows = 3
Columns = 4

my_array = [[column*row for column in range(Columns)] for row in range(Rows)]
print(my_array)

"""
12. Write a Python program that accepts a sequence of lines (blank line to
terminate) as input and prints the lines as output (all characters in lower case).
"""
lines = []
while True:
    line = input()
    if line:
        lines.append(line.upper())
    else:
        break
for line in lines:
    print(line)

"""
13. Write a Python program which accepts a sequence of comma separated
4 digit binary numbers as its input and print the numbers that are divisible
by 5 in a comma separated sequence. 
Sample Data : 0100,0011,1010,1001,1100,1001
Expected Output : 1010
"""

my_bins = input('enter some bins: ')
input_bins = my_bins.split(',')
output_bins = []

for i in input_bins:
    if int(i, base=2)%5 == 0:
        output_bins.append(i)

print(','.join(output_bins))

"""
14. Write a Python program that accepts a string and calculate the number
of digits and letters. 
Sample Data : Python 3.2
Expected Output :
Letters 6 
Digits 2
"""

my_string = input('enter something: ')
count_digits = 0
count_alpha = 0
for i in my_string:
    if i.isdigit():
        count_digits += 1
    if i.isalpha():
        count_alpha += 1

print('%d letters, %d digits' % (count_alpha, count_digits))

"""
15. Write a Python program to check the validity of password input by users.
Validation :

At least 1 letter between [a-z] and 1 letter between [A-Z].
At least 1 number between [0-9].
At least 1 character from [$#@].
Minimum length 6 characters.
Maximum length 16 characters.
"""

password = input('please enter a valid pasword: ')
while True:
    length, digits, lowers, uppers, characters = 0, 0, 0, 0, 0
    if len(password) > 6 and len(password) < 16:
        length = 1
    for i in password:
        if i.isdigit():
            digits = 1
            continue
        if i.islower():
            lowers = 1
            continue
        if i.isupper():
            uppers = 1
            continue
        if i in '$#@':
            characters = 1
            continue
    rules = [length, digits, lowers, uppers, characters]
    if all(rules):
        print('your password is good')
        break
    else:
        print('your password is not good, try again')
        password = input('please enter a valid pasword: ')

"""
16. Write a Python program to find numbers between 100 and 400 (both included)
where each digit of a number is an even number. The numbers obtained should be
printed in a comma-separated sequence.
"""
evens_list = []
for i in range(100, 401):
    number = i
    while number > 0:
        digit = number%10
        if digit%2 == 0:
            number = number//10
            even = True
        else:
            even = False
            break
    if even:
        evens_list.append(i)
print(', '.join(map(str, evens_list)))

"""
17. Write a Python program to print alphabet pattern 'A'. 
Expected Output:
  ***                                                                   
 *   *                                                                  
 *   *                                                                  
 *****                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first row of A
    if i == 1:
        print(space + STAR * (width-2))
    # middle row of A
    elif i == int(height/2+1):
        print(STAR * width)
    # rest of A
    else:
        print(STAR + space * (width-2) + STAR)

"""
18. Write a Python program to print alphabet pattern 'D'.
Expected Output:
 ****                                                                   
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 ****
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and last row of D
    if i == 1 or i == height:
        print(STAR * (width-1))
    # rest of D
    else:
        print(STAR + space * (width-2) + STAR)

"""
19. Write a Python program to print alphabet pattern 'E'.
Expected Output:
 *****                                                                  
 *                                                                      
 *                                                                      
 ****                                                                   
 *                                                                      
 *                                                                      
 *****
"""

width = 5
height = width + 2
STAR = '*'
for i in range(1, height+1):
    # first and last row of E
    if i == 1 or i == height:
        print(STAR * width)
    # middle of E
    elif i == int(height/2+1):
        print(STAR * (width-1))
    # rest of E
    else:
        print(STAR)

"""
20. Write a Python program to print alphabet pattern 'G'.
Expected Output:
  ***                                                                   
 *   *                                                                  
 *                                                                      
 * ***                                                                  
 *   *                                                                  
 *   *                                                                  
  *** 
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and last row of G
    if i == 1 or i == height:
        print(space + STAR * (width-2))
    # row above middle of G except the second row
    elif i > 2 and i <= int(height/2):
        print(STAR)
    # middle of G
    elif i == int(height/2+1):
        print(STAR + space + STAR * (width-2))
    # rest of G
    else:
        print(STAR + space * (width-2) + STAR)

"""
21. Write a Python program to print alphabet pattern 'L'.
Expected Output:
 *                                                                      
 *                                                                      
 *                                                                      
 *                                                                      
 *                                                                      
 *                                                                      
 *****
"""

width = 5
height = width + 2
STAR = '*'
for i in range(1, height+1):
    # last row of G
    if i == height:
        print(STAR * width)
    # rest of G
    else:
        print(STAR)

"""
22. Write a Python program to print alphabet pattern 'M'. 
Expected Output:
  *       *                                                             
  *       *                                                             
  * *   * *                                                             
  *   *   *                                                             
  *       *                                                             
  *       *                                                             
  *       *
"""

width = 5 # must be an odd number for symmetry
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # middle of M
    if i == int(height/2+1):
        print(STAR + space * int((width-3)/2) + STAR + space * int((width-3)/2) + STAR)
    # row above middle of M
    elif i < int(height/2+1) and i > (int(height/4+1)):
        print(STAR + space * (i-3) + STAR + space * (width-4-2*(i-3)) + STAR + space * (i-3) + STAR)
    # rest of M
    else:
        print(STAR + space * (width-2) + STAR)

"""
23. Write a Python program to print alphabet pattern 'O'. 
Expected Output:
  ***                                                                   
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
  ***
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and last row of O
    if i == 1 or i == height:
        print(space + STAR * (width-2))
    # rest of D
    else:
        print(STAR + space * (width-2) + STAR)

"""
24. Write a Python program to print alphabet pattern 'P'.
Expected Output:
 ****                                                                   
 *   *                                                                  
 *   *                                                                  
 ****                                                                   
 *                                                                      
 *                                                                      
 *
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and middle row of P
    if i == 1 or i == int(height/2+1):
        print(STAR * (width-1))
    # rows between first and middle row of P
    elif i > 1 and i < int(height/2+1):
        print(STAR + space * (width-2) + STAR)
    # lower part of P
    else:
        print(STAR)

"""
25. Write a Python program to print alphabet pattern 'R'.
Expected Output:
 ****                                                                   
 *   *                                                                  
 *   *                                                                  
 ****                                                                   
 * *                                                                    
 *  *                                                                   
 *   *
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and middle row of R
    if i == 1 or i == int(height/2+1):
        print(STAR * (width-1))
    # rows between first and middle row of R
    elif i > 1 and i < int(height/2+1):
        print(STAR + space * (width-2) + STAR)
    # lower part of R
    else:
        print(STAR + space * (i-4) + STAR)

"""
26. Write a Python program to print the following patterns. 
Expected Output:
  ****                                                                  
 *                                                                      
 *                                                                      
  ***                                                                   
     *                                                                  
     *                                                                  
 ****

ooooooooooooooooo                                                       
ooooooooooooooooo                                                       
ooooooooooooooooo                                                       
oooo                                                                    
oooo                                                                    
oooo                                                                    
ooooooooooooooooo                                                       
ooooooooooooooooo                                                       
ooooooooooooooooo                                                       
             oooo                                                       
             oooo                                                       
             oooo                                                       
ooooooooooooooooo                                                       
ooooooooooooooooo                                                       
ooooooooooooooooo 
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first row of S
    if i == 1:
        print(space + STAR * (width-1))
    # middle row of S
    elif i == int(height/2+1):
        print(space + STAR * (width-2) + space)
    # last row of S
    elif i == height:
        print(STAR * (width-1) + space)
    # rows above middle row of S (exept first row)
    elif i > 1 and i < int(height/2+1):
        print(STAR)
    else:
        print(space * (width-1) + STAR)

width = 4
height = width + 1
STAR = 'ooo'
space = ' '
for i in range(1, height+1):
    # first, last and middle row of S
    if i == 1 or i == height or i == int(height/2+1):
        for j in range(3):
            print(STAR * width)
    # rows between first and middle row of S
    elif i > 1 and i < int(height/2+1):
        for j in range(3):
            print(STAR)
    # rows below middle of S
    else:
        for j in range(3):
            print(space * (width-1)*3 + STAR)

"""
27. Write a Python program to print alphabet pattern 'T'. 
Expected Output:
 *****                                                                  
   *                                                                    
   *                                                                    
   *                                                                    
   *                                                                    
   *                                                                    
   *  
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first row of T
    if i == 1:
        print(STAR * width)
    # rest of T
    else:
        print(space * int((width-1)/2) + STAR + space * int((width-1)/2))

"""
28. Write a Python program to print alphabet pattern 'U'. 
Expected Output:
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
 *   *                                                                  
  *** 
"""

width = 5
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # last row of U
    if i == height:
        print(space + STAR * (width-2) + space)
    else:
        print(STAR + space * (width-2) + STAR)

"""
29. Write a Python program to print alphabet pattern 'X'.
Expected Output:
 *   *                                                                  
 *   *
  * *                                                                   
   *                                                                    
  * *                                                                   
 *   *                                                                  
 *   *
"""

width = 13  # must be an odd number for symmetry
height = width + 2
STAR = '*'
space = ' '
for i in range(1, height+1):
    # middle of X
    if i == int(height/2+1):
        print(space * int((width-1)/2) + STAR + space * int((width-1)/2))
        
    # row above middle of X
    elif i < int(height/2+1) and i > int(height/4):
        print(space * (i-2) + STAR + space * (width-2-2*(i-2)) + STAR + space * (i-2))
        
    # row below middle of X
    elif i > int(height/2+1) and i <= int(height*3/4+1):
        print(space * (width-i+1) + STAR + space * (width-2-2*(width-i+1))+ STAR + space * (width-i+1))
        
    # rest of M (first rows and last rows)
    else:
        print(STAR + space * (width-2) + STAR)

"""
30. Write a Python program to print alphabet pattern 'Z'. 
Expected Output:
*******                                                                 
     *                                                                  
    *                                                                   
   *                                                                    
  *                                                                     
 *                                                                      
*******
"""

width = 7
height = width
STAR = '*'
space = ' '
for i in range(1, height+1):
    # first and last row of Z
    if i == 1 or i == height:
        print(STAR * width)
    # rest of Z
    else:
        print(space * (width-i) + STAR + space * (i-1))

"""
31. Write a Python program to calculate a dog's age in dog's years. 
Note: For the first two years, a dog year is equal to 10.5 human years.
After that, each dog year equals 4 human years.
Expected Output:

Input a dog's age in human years: 15                                    
The dog's age in dog's years is 73
"""
dog_age = float(input("Input a dog's age in human years: "))
dogs_years = float()
if dog_age <= 2:
    dogs_years = dog_age*10.5
else:
    dogs_years += 21 + (dog_age-2)*4
print("The dog's age in dog's years is %.1f" % dogs_years)
                
"""
32. Write a Python program to check whether an alphabet is
a vowel or consonant. 
Expected Output:
Input a letter of the alphabet: k                                       
k is a consonant.
"""

letter = input("Input a letter of the alphabet: ").lower()
while not letter.isalpha():
    print("the character you have entered is not an english leter, try again")
    letter = input("Input a letter of the alphabet: ").lower()
if letter in 'aeiouy':      # i include y in vowels
    print(letter, 'is a vowel')
else:
    print(letter, 'is a consonant')

"""
33. Write a Python program to convert month name to a number of days. 
Expected Output:
List of months: January, February, March, April, May, June, July, August
, September, October, November, December                                
Input the name of Month: February                                       
No. of days: 28/29 days
"""

months = ['January', 'February', 'March', 'April', 'May', 'June', 'July',
          'August', 'September', 'October', 'November', 'December']
month = input('please enter a month ').title()
if month in months:
    if month == 'February':
        print('No. of days: 28/29 days')
    elif month in ('April', 'June', 'September', 'November'):
        print('No. of days: 30 days')
    else:
        print('No. of days: 31 days')
else:
    print('you enter a wrong month name')

"""
34. Write a Python program to sum of two given integers.
However, if the sum is between 15 to 20 it will return 20.
"""

int1 = int(input('integer 1: '))
int2 = int(input('integer 2: '))
def sum_of_integers(i, j):
    sum_of_int = i+j
    if sum_of_int in range(15, 21):
        return 20
    else:
        return sum_of_int
print(sum_of_integers(int1, int2))

"""
35. Write a Python program to check a string represent an integer or not.
Expected Output:
Input a string: Python                                                  
The string is not an integer.
"""

my_string = input('Input a string: ')
try:
    int(my_string)
    print('The string is an integer')
except:
    print('The string is not an integer')

"""
36. Write a Python program to check a triangle is equilateral,
isosceles or scalene. 
Note:
An equilateral triangle is a triangle in which all three sides are equal.
A scalene triangle is a triangle that has three unequal sides.
An isosceles triangle is a triangle with (at least) two equal sides.
Expected Output:
Input lengths of the triangle sides:                                    
x: 6                                                                    
y: 8                                                                    
z: 12                                                                   
Scalene triangle
"""

x = int(input('x: '))
y = int(input('y: '))
z = int(input('z: '))

if x == y == z:
    print('equilateral')
elif x != y != z:
    print('scalene')
else:
    print('isosceles')

"""
40. Write a Python program to find the median of three values.
Expected Output:
Input first number: 15                                                  
Input second number: 26                                                 
Input third number: 29                                                  
The median is 26.0
"""

x = int(input('Input first number: '))
y = int(input('Input second number: '))
z = int(input('Input third number: '))

if (x>=y and x<=z) or (x>=z and x<=y):
    median = x
elif (y>=x and y<=z) or (y>=z and y<=x):
    median = y
else:
    median = z
print('The median is', median)

"""
42. Write a Python program to calculate the sum and average of n integer
numbers (input from the user). Input 0 to finish. 
"""

number = int(input('input integer: '))
sum_integers = number
n = 1

while number != 0:
    number = int(input('input integer: '))
    sum_integers += number
    n += 1

average = sum_integers/(n-1)
print('sum is ', sum_integers)
print('average is ', average)

"""
43. Write a Python program to create the multiplication table
(from 1 to 10) of a number. 
Expected Output:
Input a number: 6                                                       
6 x 1 = 6                                                               
6 x 2 = 12                                                              
6 x 3 = 18                                                              
6 x 4 = 24                                                              
6 x 5 = 30                                                              
6 x 6 = 36                                                              
6 x 7 = 42                                                              
6 x 8 = 48                                                              
6 x 9 = 54                                                              
6 x 10 = 60
"""

number = int(input('Input a number: '))

for i in range(1, 11):
    print('%d x %d = %d' % (number, i, number*i))

"""
44. Write a Python program to construct the following pattern,
using a nested loop number. 
Expected Output:
1
22
333
4444
55555
666666
7777777
88888888
999999999
"""

n = int(input('type a number: '))
for i in range(1,n+1):
    print(str(i)*i)




MapFunctions.py '''
ALL CODE SOLUTIONS BY HIGHNESS_ATHARVA (ATHARVA SHAH) 
'''

# 1. Write a Python program to triple all numbers of a given list of integers. Use Python map. 
nums = (1, 2, 3, 4, 5, 6, 7) 
print("Original list: ", nums)
result = map(lambda x: x + x + x, nums) 
print(f"\nTriple of said list numbers: {list(result)}")
print()

# 2. Write a Python program to add three given lists using Python map and lambda. 
nums1,nums2,nums3 = [1, 2, 3], [4, 5, 6], [7, 8, 9] 
print(f"Original list: \n{nums1} \n{nums2} \n{nums3}")
result=list(map(lambda x,y,z:x+y+z, nums1, nums2, nums3))
print(f"\nNew list after adding above three lists: {list(result)}")

# 3. Write a Python program to listify the list of given strings individually using Python map.
color = ['Red', 'Blue', 'Black', 'White', 'Pink'] 
result = list(map(list, color))
print(f"Original list:\n{color}\n After listify the strings are: \n {result}")

# 4. Write a Python program to create a list containing the power of said number in bases raised to the corresponding number in the index using Python map. 
bases_num, index = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print("Base numbers abd index: ")
print(bases_num)
print(index)
result = list(map(pow, bases_num, index))
print("\nPower of said number in bases raised to the corresponding number in the index:")
print(result)

# 5. Write a Python program to square the elements of a list using map() function. 
nums = [4, 5, 2, 9]
print("Original List: ",nums)
result = map(lambda x:x*x, nums)
print("Square the elements of the said list using map():")
print(list(result))

# 6. Write a Python program to convert all the characters in uppercase and lowercase and eliminate duplicate letters from a given sequence. Use map() function. 
def change_cases(s):
  return str(s).upper(), str(s).lower()
chars = {'b', 'E', 'f', 'i', 'o', 'U', 'a'}
print("Original Characters:\n",chars)
result = map(change_cases, chars)
print("\nAfter converting above characters in upper and lower cases\nand eliminating duplicate letters:")
print(set(result))

# 7. Write a Python program to add two given lists and find the difference between lists. Use map() function. 
def addition_subtraction(x, y):
    return x + y, x - y
 
nums1 = [6, 5, 3, 9]
nums2 = [0, 1, 7, 7]
print(f"Original lists: \n{nums1}\n{nums2}")
result = map(addition_subtraction, nums1, nums2)
print("\nResult:")
print(list(result)) 

# 8. Write a Python program to convert a given list of integers and a tuple of integers in a list of strings. 
nums_list = [1,2,3,4]
nums_tuple = (0, 1, 2, 3) 
print("Original list and tuple:")
print(nums_list)
print(nums_tuple)
result_list = list(map(str,nums_list))
result_tuple = tuple(map(str,nums_tuple))
print("\nList of strings:")
print(result_list)
print("\nTuple of strings:")
print(result_tuple)

# 9. Write a Python program to create a new list taking specific elements from a tuple and convert a string value to integer. 
student_data  = [('Alberto Franco','15/05/2002','35kg'), ('Gino Mcneill','17/05/2002','37kg'), ('Ryan Parkes','16/02/1999', '39kg'), ('Eesha Hinton','25/09/1998', '35kg')]
print("Original data:")
print(student_data)
students_data_name = list(map(lambda x:x[0], student_data))
students_data_dob = list(map(lambda x:x[1], student_data))
students_data_weight = list(map(lambda x:int(x[2][:-2]), student_data))
print("\nStudent name:")
print(students_data_name)
print("Student name:")
print(students_data_dob)
print("Student weight:")
print(students_data_weight) 

# 10. Write a Python program to compute the square of first N Fibonacci numbers, using map function and generate a list of the numbers. 
import itertools
n = 10
def fibonacci_nums(x=0, y=1):
    yield x
    while True:
        yield y
        x, y = y, x + y
print("First 10 Fibonacci numbers:")
result = list(itertools.islice(fibonacci_nums(), n))
print(result)
square = lambda x: x * x 
print("\nAfter squaring said numbers of the list:")
print(list(map(square, result)))

# 11. Write a Python program to compute the sum of elements of an given array of integers, use map() function. 
from array import array
def array_sum(nums_arr):
  return sum(nums_arr)

nums = array('i', [1, 2, 3, 4, 5, -15])
print("Original array:",nums)
nums_arr = list(map(int, nums))
result = array_sum(nums_arr)
print("Sum of all elements of the said array:")
print(result)
 
# 12. Write a Python program to find the ration of positive numbers, negative numbers and zeroes in an array of integers. 
from array import array
def plusMinus(nums):
    n = len(nums)
    n1 = n2 = n3 = 0
    for x in nums:
        if x > 0:
            n1 += 1
        elif x < 0:
            n2 += 1
        else:
            n3 += 1
    return round(n1/n,2), round(n2/n,2), round(n3/n,2)

nums = array('i', [0, 1, 2, -1, -5, 6, 0, -3, -2, 3, 4, 6, 8])
print("Original array:",nums)
nums_arr = list(map(int, nums))
result = plusMinus(nums_arr)
print("Ratio of positive numbers, negative numbers and zeroes: ", result)
nums = array('i', [2, 1, 2, -1, -5, 6, 4, -3, -2, 3, 4, 6, 8])
print("\nOriginal array:",nums)
nums_arr = list(map(int, nums))
result = plusMinus(nums_arr)
print("Ratio of positive numbers, negative numbers and zeroes: ", result)

# 13. Write a Python program to count the same pair in two given lists. use map() function. 
def isSame(x,y):
  return x == y

def count_same_pair(nums1, nums2):
  return sum(map(isSame, nums1, nums2))

nums1 = [1,2,3,4,5,6,7,8]
nums2 = [2,2,3,1,2,6,7,9]
print("Original lists:")
print(nums1)
print(nums2)
print("\nNumber of same pair of the said two given lists:")
print(count_same_pair(nums1, nums2)) 

# 14. Write a Python program to interleave two given list into another list randomly using map() function. 
import random
def randomly_interleave(nums1, nums2):
  return list(
      map(
          next,
          random.sample(
              [iter(nums1)] * len(nums1) + [iter(nums2)] * len(nums2),
              len(nums1) + len(nums2),
          ),
      ))
nums1 = [1,2,7,8,3,7]
nums2 = [4,3,8,9,4,3,8,9]
print("Original lists:") 
print(nums1)
print(nums2)
print("\nInterleave two given list into another list randomly:")
print(randomly_interleave(nums1, nums2)) 

# 15. Write a Python program to split a given dictionary of lists into list of dictionaries using map function. 
def list_of_dicts(marks):
    result = map(dict, zip(*[[(key, val) for val in value] for key, value in marks.items()]))
    return list(result)
marks = {'Science': [88, 89, 62, 95], 'Language': [77, 78, 84, 80]}
print("Original dictionary of lists:")
print(marks)
print("\nSplit said dictionary of lists into list of dictionaries:")
print(list_of_dicts(marks))

# 16. Write a Python program to convert a given list of strings into list of lists using map function. 
'''ALREADY COVERED ABOVE'''

# 17. Write a Python program to convert a given list of tuples to a list of strings using map function
def tuples_to_list_string(lst):
  return list(map(' '.join, lst))   

colors = [('red', 'pink'), ('white', 'black'), ('orange', 'green')]
print("Original list of tuples:", colors)
print("\nConvert the said list of tuples to a list of strings:")
print(tuples_to_list_string(colors))

names = [('Sheridan','Gentry'), ('Laila','Mckee'), ('Ahsan','Rivas'), ('Conna','Gonzalez')]
print("\nOriginal list of tuples:", names)
print("\nConvert the said list of tuples to a list of strings:")
print(tuples_to_list_string(names))




Math.py # 1. Write a Python program to convert degree to radian. 
# Note : The radian is the standard unit of angular measure, used in many areas of mathematics. An angle's measurement in radians is numerically equal to the length of a corresponding arc of a unit circle; one radian is just under 57.3 degrees (when the arc length is equal to the radius).
# Test Data:
# Degree : 15
# Expected Result in radians: 0.2619047619047619


# 2. Write a Python program to convert radian to degree. 
# Test Data:
# Radian : .52
# Expected Result : 29.781818181818185


# 3. Write a Python program to calculate the area of a trapezoid. 
# Note : A trapezoid is a quadrilateral with two sides parallel. The trapezoid is equivalent to the British definition of the trapezium. An isosceles trapezoid is a trapezoid in which the base angles are equal so.
# Test Data:
# Height : 5
# Base, first value : 5
# Base, second value : 6
# Expected Output: Area is : 27.5


# 4. Write a Python program to calculate the area of a parallelogram. 
# Note : A parallelogram is a quadrilateral with opposite sides parallel (and therefore opposite angles equal). A quadrilateral with equal sides is called a rhombus, and a parallelogram whose angles are all right angles is called a rectangle.
# Test Data:
# Length of base : 5
# Height of parallelogram : 6
# Expected Output: Area is : 30.0


# 5. Write a Python program to calculate surface volume and area of a cylinder. 
# Note: A cylinder is one of the most basic curvilinear geometric shapes, the surface formed by the points at a fixed distance from a given straight line, the axis of the cylinder.
# Test Data:
# volume : Height (4), Radius(6)
# Expected Output:
# Volume is : 452.57142857142856
# Surface Area is : 377.1428571428571


# 6. Write a Python program to calculate surface volume and area of a sphere. 
# Note: A sphere is a perfectly round geometrical object in three-dimensional space that is the surface of a completely round ball.
# Test Data:
# Radius of sphere : .75

# Surface Area is : 7.071428571428571
# Volume is : 1.7678571428571428


# 7. Write a Python program to calculate arc length of an angle. 
# Note: In a planar geometry, an angle is the figure formed by two rays, called the sides of the angle, sharing a common endpoint, called the vertex of the angle. Angles formed by two rays lie in a plane, but this plane does not have to be a Euclidean plane.
# Test Data:
# Diameter of a circle : 8
# Angle measure : 45

# Arc Length is : 3.142857142857143


# 8. Write a Python program to calculate the area of the sector. 
# Note: A circular sector or circle sector, is the portion of a disk enclosed by two radii and an arc, where the smaller area is known as the minor sector and the larger being the major sector.
# Test Data:
# Radius of a circle : 4
# Angle measure : 45
# Expected Output:
# Sector Area: 6.285714285714286


# 9. Write a Python program to calculate the discriminant value. 
# Note: The discriminant is the name given to the expression that appears under the square root (radical) sign in the quadratic formula.
# Test Data:
# The x value : 4
# The y value : 0
# The z value : -4
# Expected Output:
# Two Solutions. Discriminant value is : 64.0


# 10. Write a Python program to find the smallest multiple of the first n numbers. Also, display the factors. 
# Test Data:
# If n = (13)

# [13, 12, 11, 10, 9, 8, 7]
# 360360


# 11. Write a Python program to calculate the difference between the squared sum of first n natural numbers and the sum of squared first n natural numbers.(default value of number=2). 
# Test Data:
# If sum_difference(12)

# 5434


# 12. Write a Python program to calculate the sum of all digits of the base to the specified power. 
# Test Data:
# If power_base_sum(2, 100)

# 115


# 13. Write a Python program to find out, if the given number is abundant. 
# Note: In number theory, an abundant number or excessive number is a number for which the sum of its proper divisors is greater than the number itself. The integer 12 is the first abundant number. Its proper divisors are 1, 2, 3, 4 and 6 for a total of 16.
# Test Data:
# If is_abundant(12)
# If is_abundant(13)
# Expected Output:
# True
# False


# 14. Write a Python program to sum all amicable numbers from 1 to specified numbers. 
# Note: Amicable numbers are two different numbers so related that the sum of the proper divisors of each is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself. For example, the proper divisors of 6 are 1, 2, and 3.)
# Test Data:
# If amicable_numbers_sum(9999)
# If amicable_numbers_sum(999)
# If amicable_numbers_sum(99)
# Expected Output:
# 31626
# 504
# 0


# 15. Write a Python program to returns sum of all divisors of a number. 
# Test Data:
# If number = 8
# If number = 12
# Expected Output:
# 7
# 16


# 16. Write a Python program to print all permutations of a given string (including duplicates). 


# 17. Write a Python program to print the first n Lucky Numbers. 
# Lucky numbers are defined via a sieve as follows.
# Begin with a list of integers starting with 1 :
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, . . . .
# Now eliminate every second number :
# 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, ...
# The second remaining number is 3, so remove every 3rd number:
# 1, 3, 7, 9, 13, 15, 19, 21, 25, ...
# The next remaining number is 7, so remove every 7th number:
# 1, 3, 7, 9, 13, 15, 21, 25, ...
# Next, remove every 9th number and so on.
# Finally, the resulting sequence is the lucky numbers.


# 18. Write a Python program to computing square roots using the Babylonian method. 
# Perhaps the first algorithm used for approximating √S is known as the Babylonian method, named after the Babylonians, or "Hero's method", named after the first-century Greek mathematician Hero of Alexandria who gave the first explicit description of the method. It can be derived from (but predates by 16 centuries) Newton's method. The basic idea is that if x is an overestimate to the square root of a non-negative real number S then S / x will be an underestimate and so the average of these two numbers may reasonably be expected to provide a better approximation.


# 19. Write a Python program to multiply two integers without using the * operator in python. 


# 20. Write a Python program to calculate magic square. 
# A magic square is an arrangement of distinct numbers (i.e., each number is used once), usually integers, in a square grid, where the numbers in each row, and in each column, and the numbers in the main and secondary diagonals, all add up to the same number, called the "magic constant." A magic square has the same number of rows as it has columns, and in conventional math notation, "n" stands for the number of rows (and columns) it has. Thus, a magic square always contains n2 numbers, and its size (the number of rows [and columns] it has) is described as being "of order n".
# Calculate magic square


# 21. Write a Python program to print all primes (Sieve_of_Eratosthenes) smaller than or equal to a specified number. 
# In mathematics, the sieve of Eratosthenes, one of a number of prime number sieves, is a simple, ancient algorithm for finding all prime numbers up to any given limit. It does so by iteratively marking as composite (i.e., not prime) the multiples of each prime, starting with the multiples of 2.


# 22. Write a python program to find the next smallest palindrome of a specified number. 


# 23. Write a python program to find the next previous palindrome of a specified number. 


# 24. Write a Python program to convert a float to ratio. 



# 21/5 



# 25. Write a Python program for nth Catalan Number. 
# In combinatorial mathematics, the Catalan numbers form a sequence of natural numbers that occur in various counting problems, often involving recursively-defined objects. They are named after the Belgian mathematician Eugène Charles Catalan (1814 –1894).


# 26. Write a Python program to print number with commas as thousands separators. 


# 27. Write a Python program to calculate the distance between two points using latitude and longitude. 



# Input coordinates of two points:                                        
# Starting latitude: 23.5                                                 
# Ending longitude: 67.5                                                  
# Starting latitude: 25.3                                                 
# Ending longitude: 69.5                                                  
# The distance is 284.73km.



# 28. Write a Python program to calculate the area of regular polygon. 



# Input number of sides: 4                                                
# Input the length of a side: 25                                          
# The area of the polygon is:  625.0000000000001



# 29. Write a Python program to calculate wind chill index. 



# Input wind speed in kilometers/hour: 150                                
# Input air temperature in degrees Celsius: 29                            
# The wind chill index is 31 



# 30. Write a Python program to find the roots of a quadratic function. 



# Quadratic function : (a * x^2) + b*x + c                                
# a: 25                                                                   
# b: 64                                                                   
# c: 36                                                                   
# There are 2 roots: -0.834579 and -1.725421



# 31. Write a Python program to convert a binary number to decimal number. 



# Input a binary number: 101011                                           
# The decimal value of the number is 43



# 32. Write a Python program to print a complex number and its real and imaginary parts. 



# Complex Number:  (2+3j)                                                          
# Complex Number - Real part:  2.0                                                 
# Complex Number - Imaginary part:  3.0



# 33. Write a Python program to add, subtract, multiply and division of two complex numbers. 



# Addition of two complex numbers :  (7-4j)                                        
# Subtraction of two complex numbers :  (1+10j)                                    
# Multiplication of two complex numbers :  (33-19j)                                
# Division of two complex numbers :  (-0.15517241379310348+0.6379310344827587j)



# 34. Write a Python program to get the length and the angle of a complex number. 



# Length of a complex number:  5.0                                                 
# Complex number Angle:  1.5707963267948966



# 35. Write a Python program to convert to/from rectangular coordinates to Polar coordinates. 



# Polar Coordinates:  (5.0, 0.9272952180016122)                                    
# Polar to rectangular:  (-2+2.4492935982947064e-16j)



# 36. Write a Python program to find the maximum and minimum numbers from the specified decimal numbers. 

# Decimal numbers : 2.45, 2.69, 2.45, 3.45, 2.00, 0.04, 7.25



# Maximum:  7.25                                                                  
# Minimum:  0.04



# 37. Write a Python program to find the sum of the following decimal numbers and display the numbers in sorted order. 

# Decimal numbers : 2.45, 2.69, 2.45, 3.45, 2.00, 0.04, 7.25



# Sum:  20.33                                                                  
# Sorted order:  [Decimal('0.04'), Decimal('2.00'), Decimal('2.45'), Decimal('2.45'
# ), Decimal('2.69'), Decimal('3.45'), Decimal('7.25')]



# 38. Write a Python program to get the square root and exponential of a given decimal number. 

# Decimal number : 1.44



# Square root of  1.44  is : 1.2                                                   
# exponential of  1.44  is : 4.220695816996552825673328929



# 39. Write a Python program to retrieve the current global context (public properties) for all decimal. 



# Emax     = 999999                                                                
# Emin     = -999999                                                               
# capitals = 1                                                                  
# prec     = 28                                                                  
# rounding = ROUND_HALF_EVEN                                                       
# flags    = <class 'decimal.InvalidOperation'>: False  
# ........



# 40. Write a Python program to round a specified decimal by setting precision (between 1 and 4). 

# Sample Number : 0.26598
# Original Number : 0.26598
# Precision- 1 : 0.3
# Precision- 2 : 0.27
# Precision- 3 : 0.266
# Precision- 4 : 0.2660



# Original Number :  0.26598                                                       
# Precision- 1 : 0.3                                                               
# Precision- 2 : 0.27                                                              
# Precision- 3 : 0.266                                                             
# Precision- 4 : 0.2660



# 41. Write a Python program to round a specified number upwards towards infinity and down towards negative infinity of precision 4. 



# 1/17 =  0.05882352941176470588235294118                                          
# Precision:  4                                                                  
# Round upwards towards infinity:  0.05883                                         
# Round down towards negative infinity:  0.05882



# 42. Write a Python program to get the local and default precision. 



# Local precision: 2                                                               
# 22/7 = 3.1        
# Default precision: 28                                                            
# 22 /7 = 3.142857142857142857142857143



# 43. Write a Python program to display the fraction instances of the string representation of a number. 

# Sample data : '0.7', '2.5', '9.32', '7e-1'



#  0.7 = 7/10                                                                  
#  2.5 = 5/2                                                                  
# 9.32 = 233/25                                                                  
# 7e-1 = 7/10



# 44. Write a Python program to create the fraction instances of float numbers. 

# Sample numbers: 0.2, 0.7, 6.5, 6.0



# 0.2 = 3602879701896397/18014398509481984                                         
# 0.7 = 3152519739159347/4503599627370496                                          
# 6.5 = 13/2                                                                  
# 6.0 = 6



# 45. Write a Python program to create the fraction instances of decimal numbers. 

# Sample decimal.2' number: Decimal('0), Decimal('0.7'), Decimal('2.5'), Decimal('3.0')



# 0.2 = 1/5                                                                  
# 0.7 = 7/10                                                                  
# 2.5 = 5/2                                                                  
# 3.0 = 3



# 46. Write a Python program to add, subtract, multiply and divide two fractions. 



# 2/3 + 3/7 = 23/21                                                                
# 2/3 - 3/7 = 5/21                                                                 
# 2/3 * 3/7 = 2/7                                                                  
# 2/3 / 3/7 = 14/9 



# 47. Write a Python program to convert a floating point number (PI) to an approximate rational value on the various denominator. 

# Note: max_denominator=1000000



# PI       = 3.141592653589793                                                     
# No limit = 3141592653589793/1000000000000000                                     
#        1 = 3                                                          
#        5 = 16/5                                                                  
#       50 = 22/7                                                                  
#       90 = 267/85                                                                
#      100 = 311/99                                                                
#      500 = 355/113                                                               
#  1000000 = 3126535/995207 



# 48. Write a Python program to generate random float numbers in a specific numerical range. 



#  2.036                                                                  
#  36.572                                                                  
#  36.557                                                                  
#  98.051                                                                  
#  37.290                                                                  
#  77.583 



# 49. Write a Python program to generate random integers in a specific numerical range. 



# 24 12 72 13 56 80



# 50. Write a Python program to generate random even integers in a specific numerical range. 



# 44 50 46 62 94 14 



# 51. Write a Python program to get a single random element from a specified string. 



# h



# 52. Write a Python program to shuffle the following elements randomly. 

# Sample elements : [1, 2, 3, 4, 5, 6, 7]



# [2, 1, 7, 5, 3, 4, 6]



# 53. Write a Python program to flip a coin 1000 times and count heads and tails. 



# Heads: 5073

# Tails: 4927 



# 54. Write a Python program to print a random sample of words from the system dictionary. 



# cellophane's                                            
# matter's                                                       
# Whiteley's                                                     
# airdrop's                                                      
# sulkiest                                                       
# whisper's                                                      
# downturns 



# 55. Write a Python program to randomly select an item from a list. 



# Red 



# 56. Write a Python program to calculate the absolute value of a floating point number. 



# 2.1                                                                  
# 0.0                                                                  
# 10.1                                                                  
# 0.0 



# 57. Write a Python program to calculate the standard deviation of the following data. 



# Sample Data:  [4, 2, 5, 8, 6]                                                    
# Standard Deviation :  2.23606797749979



# 58. Write a Python program to print the floating point from mantissa, exponent pair. 



# Mantissa  Exponent  Floating point value                                         
# --------  --------  --------------------                                         
#    0.70       -3     0.09                                                        
#    0.30        0     0.30                                                        
#    0.50        3     4.00 



# 59. Write a Python program to split fractional and integer parts of a floating point number. 



#            (F)  (I)                                                              
# 0/2 = 0.0 (0.0, 0.0)                                                             
# 1/2 = 0.5 (0.5, 0.0)                                                             
# 2/2 = 1.0 (0.0, 1.0)                                                             
# 3/2 = 1.5 (0.5, 1.0)                                                             
# 4/2 = 2.0 (0.0, 2.0)                                                             
# 5/2 = 2.5 (0.5, 2.0)



# 60. Write a Python program to parse math formulas and put parentheses around multiplication and division. 

# Sample data : 4+5*7/2



# 4+((5*7)/2)



# 61. Write a Python program to describe linear regression. 

# Note : A linear regression line has an equation of the form Y = a + bX, where X is the explanatory variable and Y is the dependent variable. The slope of the line is b, and a is the intercept (the value of y when x = 0).



# Enter the number of data points: 2                    
# X1: 1  
# Y1: 2 
# X2: 3    
# Y2: 4  
# Best fit line:
# y = 1.0x + 1.0
# Enter a value to calculate: 12                            
# y = 13.0 



# 62. Write a Python program to calculate a grid of hexagon coordinates of the given radius given lower-left and upper-right coordinates. The function will return a list of lists containing 6 tuples of x, y point coordinates. These can be used to construct valid regular hexagonal polygons. 



# [[(-5.0, -4.196152422706632), (-5.0, -0.7320508075688767), (-2.0, 1.0), (1.0, -0.
# 7320508075688767), (1.0, -4.196152422706632), (-2.0, -5.928203230275509), (-5.0, 
# -4.196152422706632)], [(1.0, -4.196152422706632), (1.0, -0.7320508075688767), (4.
# 0, 1.0), (7.0, -0.7320508075688767), (7.0, -4.196152422706632)....... 



# 63. Write a Python program to create a simple math quiz. 



# ************************                                                         
# ** A Simple Math Quiz **                                                         
# ************************                                                         
# 1. Addition                                                                      
# 2. Subtraction                                                                   
# 3. Multiplication                                                                
# 4. Integer Division                                                              
# 5. Exit                                                                          
# ------------------------                                                         
# Enter your choice: 1                                                             
# Enter your answer                                                                
# 1 + 5 = 6                                                                        
# Correct.                                                                         
# .........                                        
# Your score is 100.0%. Thank you. 



# 64. Write a Python program to calculate the volume of a tetrahedron. 

# Note: In geometry, a tetrahedron (plural: tetrahedra or tetrahedrons) is a polyhedron composed of four triangular faces, six straight edges, and four vertex corners. The tetrahedron is the simplest of all the ordinary convex polyhedra and the only one that has fewer than 5 faces.



# 117.85



# 65. Write a Python program to compute the value of e(2.718281827...) using infinite series. 



# The mathematical constant e                                                      
# 2.7182818282861687                                                               
# 2.718281828459045 



# 66. Write a Python program to create an ASCII waveform. 



#    #                                                    
#                                                  *                               
                                                                                 
#                                  #                                               
#                                                *                                 
                                                                                 
# .......
			                               
#                                      #                                           
#                                               *  



# 67. Write a Python program to create a dot string. 


# 68. Write a Python program to create a Pythagorean theorem calculator. 

# Note : In mathematics, the Pythagorean theorem, also known as Pythagoras' theorem, is a fundamental relation in Euclidean geometry among the three sides of a right triangle. It states that the square of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the other two sides.



# Pythagorean theorem calculator! Calculate your triangle sides.                   
# Assume the sides are a, b, c and c is the hypotenuse (the side opposite the right
#  angle                                                                           
# Which side (a, b, c) do you wish to calculate? side>a                            
# Input the length of side b:10                                                    
# Input the length of side c:20                                                    
# The length of side a is                                                          
# 17.320508075688775 



# 69. Write a Python function to round up a number to specified digits. 



# Original  Number:  123.01247                                                     
# 124                                                                              
# 123.1                                                                            
# 123.02                                                                           
# 123.013



# 70. Write a Python program for casino simulation. 



# Exp 0                                                                            
# Exp 1                                                                            
# Exp 2                                                                            
# Exp 3                                                                            
# Exp 4  
# .......
# Exp 998                                                                          
# Exp 999                                                                          
# Average max amount earned 10493.144 with standard deviation 50.892644498001886



# 71. Write a Python program to reverse a range. 



# range(9, -1, -2)                                                                 
# range(4, 0, -1) 



# 72. Write a Python program to create a range for floating numbers. 



# [0.0, 0.1, 0.2, 0.30000000000000004, 0.4, 0.5, 0.6000000000000001, 0.700000000000
# 0001, 0.8, 0.9, 1.0]  
# 01, 0.7000000000000001, 0.8, 0.9, 1.0] 



# 73. Write a Python program to generate (given an integer n) a square matrix filled with elements from 1 to n raised to the power of 2 in spiral order. 



# [[1, 2, 3], [8, 9, 4], [7, 6, 5]]



# 74. Write a Python program to select a random date in the current year. 



# 2016-02-08



# 75. Write a Python program to calculate clusters using Hierarchical Clustering method. 



# Input number of points.> 2                                              
# Input point (eg. 1,1)A> 1,2                                             
# Input point (eg. 1,1)B> 3,4                                             
# Distance matrix no.1:                                                   
# [0.0, 2.83]                                                             
# [2.83, 0.0]                                                             
# Cluster is : [AB]



# 76. Write a Python program to implement Euclidean Algorithm to compute the greatest common divisor (gcd). 



# 304 = 2 * 150 + 4                                            
# 150 = 37 * 4 + 2                                            
# 4 = 2 * 2 + 0                                                       
# gcd is 2 
# .........
# 6 = 2 * 3 + 0                                                 
# gcd is 3 



# 77. Write a Python program to convert RGB color to HSV color. 



# (0, 0.0, 100.0)                                                                  
# (120.0, 100.0, 84.31372549019608) 



# 78. Write a Python program to find perfect squares between two given numbers. 



 



# 79. Write a Python program to compute Euclidean distance. 

# Note: In mathematics, the Euclidean distance or Euclidean metric is the "ordinary" (i.e. straight-line) distance between two points in Euclidean space. With this distance, Euclidean space becomes a metric space. The associated norm is called the Euclidean norm.



# Euclidean distance from x to y:  4.69041575982343



# 80. Write a Python program to convert an integer to a 2 byte Hex value. 



# 1 --> 0x01                                                                                                        
# 2 --> 0x02                                                                                                        
# 3 --> 0x03                                                                                                        
# 4 --> 0x04                                                                                                        
# 5 --> 0x05                                                                                                        
# 6 --> 0x06                                                                                                        
# 7 --> 0x07                                                                                                        
# 8 --> 0x08                                                                                                        
# 9 --> 0x09 
 



# 81. Write a Python program to generate a series of unique random numbers. 




modules.py # Python comes with a library of standard modules. Some modules are built into the interpreter; these provide access to operations that are not part of the core of the language but are nevertheless built in, either for efficiency or to provide access to operating system primitives such as system calls.


# Module - random

# 1.generate a random color hex, a random alphabetical string, random value between two integers (inclusive) and a random multiple of 7 between 0 and 70. Use random.randint()

import random
import string
print("Generate a random color hex:")
print("#{:06x}".format(random.randint(0, 0xFFFFFF)))
print("\nGenerate a random alphabetical string:")
max_length = 255
s = "".join(
    random.choice(string.ascii_letters)
    for _ in range(random.randint(1, max_length))
)
print(s)
print("Generate a random value between two integers, inclusive:")
print(random.randint(0, 10))
print(random.randint(-7, 7))
print(random.randint(1, 1))
print("Generate a random multiple of 7 between 0 and 70:")
print(random.randint(0, 10) * 7)


# 2.select a random element from a list, set, dictionary (value) and a file from a directory. Use random.choice()
import random
import os
print("Select a random element from a list:")
elements = [1, 2, 3, 4, 5]
print(random.choice(elements))
print(random.choice(elements))
print(random.choice(elements))
print("\nSelect a random element from a set:")
elements = {1, 2, 3, 4, 5}
# convert to tuple because sets are invalid inputs
print(random.choice(tuple(elements)))
print(random.choice(tuple(elements)))
print(random.choice(tuple(elements)))
print("\nSelect a random value from a dictionary:")
d = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5}
key = random.choice(list(d))
print(d[key])
key = random.choice(list(d))
print(d[key])
key = random.choice(list(d))
print(d[key])
print("\nSelect a random file from a directory.:")
print(random.choice(os.listdir("/")))



# 3.generate a random alphabetical character, alphabetical string and alphabetical string of a fixed length. Use random.choice()
import random
import string
print("Generate a random alphabetical character:")
print(random.choice(string.ascii_letters))
print("\nGenerate a random alphabetical string:")
max_length = 255
str1 = "".join(
    random.choice(string.ascii_letters)
    for _ in range(random.randint(1, max_length))
)
print(str1)
print("\nGenerate a random alphabetical string of a fixed length:")
str1 = "".join(random.choice(string.ascii_letters) for _ in range(10))
print(str1)



# 4.construct a seeded random number generator, also generate a float between 0 and 1, excluding 1. Use random.random()
import random
print("Construct a seeded random number generator:")
print(random.Random().random())
print(random.Random(0).random())
print("\nGenerate a float between 0 and 1, excluding 1:")
print(random.random())



# 5.generate a random integer between 0 and 6 - excluding 6, random integer between 5 and 10 - excluding 10, random integer between 0 and 10, with a step of 3 and random date between two dates. Use random.randrange()
import random
import datetime
print("Generate a random integer between 0 and 6:")
print(random.randrange(5))
print("Generate random integer between 5 and 10, excluding 10:")
print(random.randrange(start=5, stop=10))
print("Generate random integer between 0 and 10, with a step of 3:")
print(random.randrange(start=0, stop=10, step=3))
print("\nRandom date between two dates:")
start_dt = datetime.date(2019, 2, 1)
end_dt = datetime.date(2019, 3, 1)
time_between_dates = end_dt - start_dt
days_between_dates = time_between_dates.days
random_number_of_days = random.randrange(days_between_dates)
random_date = start_dt + datetime.timedelta(days=random_number_of_days)
print(random_date)



# 6.shuffle the elements of a given list. Use random.shuffle()

import random
nums = [1, 2, 3, 4, 5]
print("Original list:")
print(nums)
random.shuffle(nums)
print("Shuffle list:")
print(nums)
words = ['red', 'black', 'green', 'blue']
print("\nOriginal list:")
print(words)
random.shuffle(words)
print("Shuffle list:")
print(words)


# 7.generate a float between 0 and 1, inclusive and generate a random float within a specific range. Use random.uniform()

import random
print("Generate a float between 0 and 1, inclusive:")
print(random.uniform(0, 1))
print("\nGenerate a random float within a range:")
random_float = random.uniform(1.0, 3.0)
print(random_float)


# 8.create a list of random integers and randomly select multiple items from the said list. Use random.sample()
import random
print("Create a list of random integers:")
population = range(100)
nums_list = random.sample(population, 10)
print(nums_list)
no_elements = 4
print("\nRandomly select",no_elements,"multiple items from the said list:")
result_elements = random.sample(nums_list, no_elements)
print(result_elements)
no_elements = 8
print("\nRandomly select",no_elements,"multiple items from the said list:")
result_elements = random.sample(nums_list, no_elements)
print(result_elements)



# 9.set a random seed and get a random number between 0 and 1. Use random.random. 
import random
print("Set a random seed and get a random number between 0 and 1:")
random.seed(0)
new_random_value = random.random()
print(new_random_value)
random.seed(1)
new_random_value = random.random()
print(new_random_value)
random.seed(2)
new_random_value = random.random()
print(new_random_value)


# Module - types

# 1.check if a function is a user-defined function or not. Use types.FunctionType, types.LambdaType()
import types
def func(): 
    return 1

print(isinstance(func, types.FunctionType))
print(isinstance(func, types.LambdaType))
print(isinstance(lambda x: x, types.FunctionType))
print(isinstance(lambda x: x, types.LambdaType))
print(isinstance(max, types.FunctionType))
print(isinstance(max, types.LambdaType))
print(isinstance(abs, types.FunctionType))
print(isinstance(abs, types.LambdaType))



# 2.check if a given value is a method of a user-defined class. Use types.MethodType()
import types
class C:
    def x():
        return 1
    def y():
        return 1    
        
def b():
    return 2

print(isinstance(C().x, types.MethodType))
print(isinstance(C().y, types.MethodType))
print(isinstance(b, types.MethodType))
print(isinstance(max, types.MethodType))
print(isinstance(abs, types.MethodType))



# 3.check if a given function is a generator or not. Use types.GeneratorType()

import types
def a(x):
    yield x
        
def b(x):
    return x

def add(x, y):
    return x + y

print(isinstance(a(456), types.GeneratorType))
print(isinstance(b(823), types.GeneratorType))
print(isinstance(add(8,2), types.GeneratorType))


# 4.check if a given value is compiled code or not. Also check if a given value is a module or not. Use types.CodeType, types.ModuleType()

import types
print("Check if a given value is compiled code:")
code = compile("print('Hello')", "sample", "exec")
print(isinstance(code, types.CodeType))
print(isinstance("print(abs(-111))", types.CodeType))
print("\nCheck if a given value is a module:")
print(isinstance(types, types.ModuleType))


# Module - decimal

# 1.construct a Decimal from a float and a Decimal from a string. Also represent the Decimal value as a tuple. Use decimal.Decimal

import decimal
print("Construct a Decimal from a float:")
pi_val = decimal.Decimal(3.14159)
print(pi_val)
print(pi_val.as_tuple())
print("\nConstruct a Decimal from a string:")
num_str = decimal.Decimal("123.25")
print(num_str)
print(num_str.as_tuple())


# 2.configure the rounding to round up and round down a given decimal value. Use decimal.Decimal
import decimal
print("Configure the rounding to round up:")
decimal.getcontext().prec = 1
decimal.getcontext().rounding = decimal.ROUND_UP
print(decimal.Decimal(30) / decimal.Decimal(4))
print("\nConfigure the rounding to round down:")
decimal.getcontext().prec = 3
decimal.getcontext().rounding = decimal.ROUND_DOWN
print(decimal.Decimal(30) / decimal.Decimal(4))
print("\nConfigure the rounding to round up:")
print(decimal.Decimal('8.325').quantize(decimal.Decimal('.01'), rounding=decimal.ROUND_UP))
print("\nConfigure the rounding to round down:")
print(decimal.Decimal('8.325').quantize(decimal.Decimal('.01'), rounding=decimal.ROUND_DOWN))



# 3.round a Decimal value to the nearest multiple of 0.10, unless already an exact multiple of 0.05. Use decimal.Decimal

from decimal import Decimal
#Source: https://bit.ly/3hEyyY4

def round_to_10_cents(x):
    remainder = x.remainder_near(Decimal('0.10'))
    return x if abs(remainder) == Decimal('0.05') else x - remainder

# Test code.
for x in range(80, 120):
    y = Decimal(x) / Decimal('1E2')
    print("{0} rounds to {1}".format(y, round_to_10_cents(y)))


# 4.configure the rounding to round to the floor, ceiling. Use decimal.ROUND_FLOOR, decimal.ROUND_CEILING
import decimal
print("Configure the rounding to round to the floor:")
decimal.getcontext().prec = 4
decimal.getcontext().rounding = decimal.ROUND_FLOOR
print(decimal.Decimal(20) / decimal.Decimal(6))
print("\nConfigure the rounding to round to the ceiling:")
decimal.getcontext().prec = 4
decimal.getcontext().rounding = decimal.ROUND_CEILING
print(decimal.Decimal(20) / decimal.Decimal(6))



# 5.configure the rounding to round to the nearest - with ties going towards 0, with ties going away from 0. Use decimal.ROUND_HALF_DOWN, decimal.ROUND_HALF_UP
import decimal
print("Configure the rounding to round to the nearest, with ties going towards 0:")
decimal.getcontext().prec = 1
decimal.getcontext().rounding = decimal.ROUND_HALF_DOWN
print(decimal.Decimal(10) / decimal.Decimal(4))
print("\nConfigure the rounding to round to the nearest, with ties going away from 0:")
decimal.getcontext().prec = 1
decimal.getcontext().rounding = decimal.ROUND_HALF_UP
print(decimal.Decimal(10) / decimal.Decimal(4))



# 6.configure the rounding to round to the nearest, with ties going to the nearest even integer. Use decimal.ROUND_HALF_EVEN
import decimal
print("Configure the rounding to round to the nearest, with ties going to the nearest even integer:")
decimal.getcontext().prec = 1
decimal.getcontext().rounding = decimal.ROUND_HALF_EVEN
print(decimal.Decimal(10) / decimal.Decimal(4))



# 7.display a given decimal value in scientific notation. Use decimal.Decimal
import decimal
#Source: https://bit.ly/2SfZEtL
def format_e(n):
    a = '%E' % n
    return a.split('E')[0].rstrip('0').rstrip('.') + 'E' + a.split('E')[1]

print("Original decimal value: "+ "40800000000.00000000000000")
print("Scientific notation of the said decimal value:")
print(format_e(decimal.Decimal('40800000000.00000000000000')))
print("\nOriginal decimal value: "+ "40000000000.00000000000000")
print("Scientific notation of the said decimal value:")
print(format_e(decimal.Decimal('40000000000.00000000000000')))
print("\nOriginal decimal value: "+ "40812300000.00000000000000")
print("Scientific notation of the said decimal value:")
print(format_e(decimal.Decimal('40812300000.00000000000000')))



# Module - copy

# 1.create a shallow copy of a given list. Use copy.copy
import copy
nums_x = [1, [2, 3, 4]]
print("Original list: ", nums_x)
nums_y = copy.copy(nums_x)
print("\nCopy of the said list:")
print(nums_y)
print("\nChange the value of an element of the original list:")
nums_x[1][1] = 10
print(nums_x)
print("\nSecond list:")
print(nums_y)
nums =  [[1], [2]]
nums_copy = copy.copy(nums)
print("\nOriginal list:")
print(nums)
print("\nCopy of the said list:")
print(nums_copy)
print("\nChange the value of an element of the original list:")
nums[0][0] = 0
print("\nFirst list:")
print(nums)
print("\nSecond list:")
print(nums_copy)



# 2.create a deep copy of a given list. Use copy.copy
import copy
nums_x = [1, [2, 3, 4]]
print("Original list: ", nums_x)
nums_y = copy.deepcopy(nums_x)
print("\nDeep copy of the said list:")
print(nums_y)
print("\nChange the value of an element of the original list:")
nums_x[1][1] = 10
print(nums_x)
print("\nCopy of the second list (Deep copy):")
print(nums_y)
nums = [[1, 2, 3], [4, 5, 6]]
deep_copy = copy.deepcopy(nums)
print("\nOriginal list:")
print(nums)
print("\nDeep copy of the said list:")
print(deep_copy)
print("\nChange the value of some elements of the original list:")
nums[0][2] = 55
nums[1][1] = 77
print("\nOriginal list:")
print(nums)
print("\nSecond list (Deep copy):")
print(deep_copy)



# 3.create a shallow copy of a given dictionary. Use copy.copy
import copy
nums_x = {"a":1, "b":2, 'cc':{"c":3}}
print("Original dictionary: ", nums_x)
nums_y = copy.copy(nums_x)
print("\nCopy of the said list:")
print(nums_y)
print("\nChange the value of an element of the original dictionary:")
nums_x["cc"]["c"] = 10
print(nums_x)
print("\nSecond dictionary:")
print(nums_y)

nums = {"x":1, "y":2, 'zz':{"z":3}}
nums_copy = copy.copy(nums)
print("\nOriginal dictionary :")
print(nums)
print("\nCopy of the said list:")
print(nums_copy)
print("\nChange the value of an element of the original dictionary:")
nums["zz"]["z"] = 10
print("\nFirst dictionary:")
print(nums)
print("\nSecond dictionary (copy):")
print(nums_copy)



# 4.create a deep copy of a given dictionary. Use copy.copy

import copy
nums_x = {"a":1, "b":2, 'cc':{"c":3}}
print("Original dictionary: ", nums_x)
nums_y = copy.deepcopy(nums_x)
print("\nDeep copy of the said list:")
print(nums_y)
print("\nChange the value of an element of the original dictionary:")
nums_x["cc"]["c"] = 10
print(nums_x)
print("\nSecond dictionary (Deep copy):")
print(nums_y)

nums = {"x":1, "y":2, 'zz':{"z":3}}
nums_copy = copy.deepcopy(nums)
print("\nOriginal dictionary :")
print(nums)
print("\nDeep copy of the said list:")
print(nums_copy)
print("\nChange the value of an element of the original dictionary:")
nums["zz"]["z"] = 10
print("\nFirst dictionary:")
print(nums)
print("\nSecond dictionary (Deep copy):")
print(nums_copy)


# Module - csv

# 1.read and display the content of a given CSV file. Use csv.reader

import csv
reader = csv.reader(open("employees.csv"))
for row in reader:
    print(row)


# 2.count the number of lines in a given CSV file. Use csv.reader
import csv
reader = csv.reader(open("employees.csv"))
no_lines= len(list(reader))
print(no_lines)



# 3.parse a given CSV string and get the list of lists of string values. Use csv.reader

import csv
csv_string = """1,2,3
4,5,6
7,8,9
"""
print("Original string:")
print(csv_string)
lines = csv_string.splitlines()
print("List of CSV formatted strings:")
print(lines)
reader = csv.reader(lines)
parsed_csv = list(reader)
print("\nList representation of the CSV file:")
print(parsed_csv)


# 4.read the current line from a given CSV file. Use csv.reader
import csv
f = open("employees.csv", newline='')
csv_reader = csv.reader(f)
print(next(csv_reader))
print(next(csv_reader))
print(next(csv_reader))



# 5.skip the headers of a given CSV file. Use csv.reader

import csv
f = open("employees.csv", "r")
reader = csv.reader(f)
next(reader)

for row in reader:
    print(row)


# 6.write (without writing separate lines between rows) and read a CSV file with specified delimiter. Use csv.reader

import csv
with open("test.csv", "w", newline='') as fw:
    writer = csv.writer(fw, delimiter = ",")
    writer.writerow(["a","b","c"])
    writer.writerow(["d","e","f"])
    writer.writerow(["g","h","i"])
with open("test.csv", "r") as fr:
    csv = csv.reader(fr, delimiter = ",")
    for row in csv:
      print(row)
# 7.write dictionaries and a list of dictionaries to a given CSV file. Use csv.reader
import csv
print("Write dictionaries to a CSV file:")
with open("test.csv", "w", newline='') as fw:
    writer = csv.DictWriter(fw, fieldnames=["Name", "Class"])
    writer.writeheader()
    writer.writerow({"Name": "Jasmine Barrett", "Class": "V"})
    writer.writerow({"Name": "Garry Watson", "Class": "V"})
    writer.writerow({"Name": "Courtney Caldwell", "Class": "VI"})
with open("test.csv", "r") as fr:
    csv = csv.reader(fr, delimiter = ",")
    for row in csv:
      print(row)







NLTK.py # Python NLTK Corpus

# In linguistics, a corpus(plural corpora) or text corpus is a large and structured set of texts. In corpus linguistics, they are used to do statistical analysis and hypothesis testing, checking occurrences or validating linguistic rules within a specific language territory.
# Each corpus reader class is specialized to handle a specific corpus format. In addition, the nltk.corpus package automatically creates a set of corpus reader instances that can be used to access the corpora in the NLTK data package.

from nltk.tokenize import SExprTokenizer
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.tokenize import WordPunctTokenizer
from nltk.tokenize import word_tokenize
import random
from nltk.corpus import names
import nltk.data
from nltk.tokenize import TweetTokenizer
from nltk.corpus import wordnet
import nltk
from nltk.corpus import stopwords
import nltk.corpus
# 1. Write a Python NLTK program to list down all the corpus names.
dir(nltk.corpus)
print("\nAvailable corpus names:")
print(dir(nltk.corpus))

# 2. Write a Python NLTK program to get a list of common stop words in various languages in Python.
print(stopwords.fileids())

# 3. Write a Python NLTK program to check the list of stopwords in various languages.
# From Wikipedia:
# In computing, stop words are words which are filtered out before or after processing of natural language data(text). Though "stop words" usually refers to the most common words in a language, there is no single universal list of stop words used by all natural language processing tools, and indeed not all tools even use such a list. Some tools specifically avoid removing these stop words to support phrase search.
# Any group of words can be chosen as the stop words for a given purpose. For some search engines, these are some of the most common, short function words, such as the, is, at, which, and on. In this case, stop words can cause problems when searching for phrases that include them, particularly in names such as "The Who", "The The", or "Take That". Other search engines remove some of the most common words-including lexical words, such as "want"-from a query in order to improve performance.
result = set(stopwords.words('english'))
print("List of stopwords in English:")
print(result)
print("\nList of stopwords in Arabic:")
result = set(stopwords.words('arabic'))
print(result)
print("\nList of stopwords in Azerbaijani:")
result = set(stopwords.words('azerbaijani'))
print(result)
print("\nList of stopwords in Danish:")
result = set(stopwords.words('danish'))
print(result)
print("\nList of stopwords in Dutch:")
result = set(stopwords.words('dutch'))
print(result)
print("\nList of stopwords in Finnish:")
result = set(stopwords.words('finnish'))
print(result)
print("\nList of stopwords in French:")
result = set(stopwords.words('french'))
print(result)
print("\nList of stopwords in German:")
result = set(stopwords.words('german'))
print(result)
print("\nList of stopwords in Greek:")
result = set(stopwords.words('greek'))
print(result)
print("\nList of stopwords in Hungarian:")
result = set(stopwords.words('hungarian'))
print(result)
print("\nList of stopwords in Indonesian:")
result = set(stopwords.words('indonesian'))
print(result)
print("\nList of stopwords in Italian:")
result = set(stopwords.words('italian'))
print(result)
print("\nList of stopwords in Kazakh:")
result = set(stopwords.words('kazakh'))
print(result)
print("\nList of stopwords in Nepali:")
result = set(stopwords.words('nepali'))
print(result)
print("\nList of stopwords in Norwegian:")
result = set(stopwords.words('norwegian'))
print(result)
print("\nList of stopwords in Portuguese:")
result = set(stopwords.words('portuguese'))
print(result)
print("\nList of stopwords in Romanian:")
result = set(stopwords.words('romanian'))
print(result)
print("\nList of stopwords in Russian:")
result = set(stopwords.words('russian'))
print(result)
print("\nList of stopwords in Spanish:")
result = set(stopwords.words('spanish'))
print(result)
print("\nList of stopwords in Swedish:")
result = set(stopwords.words('swedish'))
print(result)
print("\nList of stopwords in Turkish:")
result = set(stopwords.words('turkish'))
print(result)


# 4. Write a Python NLTK program to remove stop words from a given text.
stoplist = stopwords.words('english')
text = '''
In computing, stop words are words which are filtered out before or after 
processing of natural language data (text). Though "stop words" usually 
refers to the most common words in a language, there is no single universal 
list of stop words used by all natural language processing tools, and 
indeed not all tools even use such a list. Some tools specifically avoid 
removing these stop words to support phrase search.
'''
print("\nOriginal string:")
print(text)
clean_word_list = [word for word in text.split() if word not in stoplist]
print("\nAfter removing stop words from the said text:")
print(clean_word_list)


# 5. Write a Python NLTK program to omit some given stop words from the stopwords list.
result = set(stopwords.words('english'))
print("List of stopwords in English:")
print(result)
print("\nOmit - 'again', 'once' and 'from':")
stop_words = set(stopwords.words('english')) - {'again', 'once', 'from'}
print("\nList of fresh stopwords in English:")
print(stop_words)


# 6. Write a Python NLTK program to find the definition and examples of a given word using WordNet.
# From Wikipedia,
# WordNet is a lexical database for the English language. It groups English words into sets of synonyms called synsets, provides short definitions and usage examples, and records a number of relations among these synonym sets or their members. WordNet can thus be seen as a combination of dictionary and thesaurus. While it is accessible to human users via a web browser, its primary use is in automatic text analysis and artificial intelligence applications. The database and software tools have been released under a BSD style license and are freely available for download from the WordNet website. Both the lexicographic data(lexicographer files) and the compiler(called grind) for producing the distributed database are available.
syns = wordnet.synsets("fight")
print("Defination of the said word:")
print(syns[0].definition())
print("\nExamples of the word in use::")
print(syns[0].examples())


# 7. Write a Python NLTK program to find the sets of synonyms and antonyms of a given word.
# From Winkled,
# WordNet is a lexical database for the English language. It groups English words into sets of synonyms called synsets, provides short definitions and usage examples, and records a number of relations among these synonym sets or their members.
synonyms = []
antonyms = []

for syn in wordnet.synsets("end"):
    for l in syn.lemmas():
        synonyms.append(l.name())
        if l.antonyms():
            antonyms.append(l.antonyms()[0].name())
print("\nSet of synonyms of the said word:")
print(set(synonyms))
print("\nSet of antonyms of the said word:")
print(set(antonyms))


# 8. Write a Python NLTK program to get the overview of the tagset, details of a specific tag in the tagset and details on several related tagsets, using regular expression.
print("Overview of the tagset:")
print(nltk.help.brown_tagset())
print("\nDetails of a specific tag :")
print(nltk.help.brown_tagset(r'NNS'))
print("\nDetails on several related tagsets, using regular expression:")
nltk.help.brown_tagset(r'WP*')


# 9. Write a Python NLTK program to compare the similarity of two given nouns.
print("\nComparing ship anb boat:")
n1 = wordnet.synset('ship.n.01')
n2 = wordnet.synset('boat.n.01')
print("\nComparing bus anb boat:")
print(n1.wup_similarity(n2))
n1 = wordnet.synset('bus.n.01')
n2 = wordnet.synset('boat.n.01')
print(n1.wup_similarity(n2))
print("\nComparing red anb greed:")
n1 = wordnet.synset('red.n.01')
n2 = wordnet.synset('green.n.01')
print(n1.wup_similarity(n2))

# 10. Write a Python NLTK program to compare the similarity of two given verbs.
print("\nComparing go anb return:")
v1 = wordnet.synset('go.v.01')
v2 = wordnet.synset('return.v.01')
print(v1.wup_similarity(v2))

print("\nComparing buy anb sell:")
v1 = wordnet.synset('buy.v.01')
v2 = wordnet.synset('sell.v.01')
print(v1.wup_similarity(v2))

print("\nComparing begin anb start:")
v1 = wordnet.synset('begin.v.01')
v2 = wordnet.synset('start.v.01')
print(v1.wup_similarity(v2))

# 11. Write a Python NLTK program to find the number of male and female names in the names corpus. Print the first 10 male and female names.
# Note: The names corpus contains a total of around 2943 male(male.txt) and 5001 female(female.txt) names. It's compiled by Kantrowitz, Ross.
from nltk.corpus import names
print("\nNumber of male names:")
print (len(names.words('male.txt')))
print("\nNumber of female names:")
print (len(names.words('female.txt')))
male_names = names.words('male.txt')
female_names = names.words('female.txt')
print("\nFirst 10 male names:")
print(male_names[:15])
print("\nFirst 10 female names:")
print(female_names[:15])


# 12. Write a Python NLTK program to print the first 15 random combine labeled male and labeled female names from names corpus.
male_names = names.words('male.txt')
female_names = names.words('female.txt')
labeled_male_names = [(str(name), 'male') for name in male_names]
labeled_female_names = [(str(name), 'female') for name in female_names]

# combine labeled male and labeled female names
labeled_all_names = labeled_male_names + labeled_female_names
# shuffle the labeled names array
random.shuffle(labeled_all_names)
print("First 15 random labeled combined names:")
print(labeled_all_names[:15])


# 13. Write a Python NLTK program to extract the last letter of all the labeled names and create a new array with the last letter of each name and the associated label.
from nltk.corpus import names
import random
male_names = names.words('male.txt')
female_names = names.words('female.txt')
labeled_male_names = [(str(name), 'male') for name in male_names]
labeled_female_names = [(str(name), 'female') for name in female_names]
# combine labeled male and labeled female names
all_labeled_names = labeled_male_names + labeled_female_names
feature_set = [(name[-1], gender) for (name, gender) in all_labeled_names]
print("\nFirst 15 labeled names:")
print((all_labeled_names[:15]))
print("\nLast letter of all the labeled names with the associated label:")
print((feature_set[:15]))


# Python NLTK Tokenize
# What is Tokenize?
# Tokenization is the process of demarcating and possibly classifying sections of a string of input characters. The resulting tokens are then passed on to some other form of processing. The process can be considered a sub-task of parsing input.

# 1. Write a Python NLTK program to split the text sentence/paragraph into a list of words.
text = '''
Joe waited for the train. The train was late. 
Mary and Samantha took the bus. 
I looked for Mary and Samantha at the bus station.
'''
print("\nOriginal string:")
print(text)
from nltk.tokenize import sent_tokenize
token_text = sent_tokenize(text)
print("\nSentence-tokenized copy in a list:")
print(token_text)
print("\nRead the list:")
for s in token_text:
    print(s)

# 2. Write a Python NLTK program to tokenize sentences in languages other than English.
text = '''
NLTK ist Open Source Software. Der Quellcode wird unter den Bedingungen der Apache License Version 2.0 vertrieben.  
Die Dokumentation wird unter den Bedingungen der Creative Commons-Lizenz Namensnennung - Nicht kommerziell - Keine 
abgeleiteten Werke 3.0 in den Vereinigten Staaten verteilt.'''
print("\nOriginal string:")
print(text)
from nltk.tokenize import sent_tokenize
token_text = sent_tokenize(text, language='german')
print("\nSentence-tokenized copy in a list:")
print(token_text)
print("\nRead the list:")
for s in token_text:
    print(s)


# 3. Write a Python NLTK program to create a list of words from a given string.
text = "Joe waited for the train. The train was late. Mary and Samantha took the bus. I looked for Mary and Samantha at the bus station."
print("\nOriginal string:")
print(text)
print("\nList of words:")
print(word_tokenize(text))


# 4. Write a Python NLTK program to split all punctuation into separate tokens.
text = "Reset your password if you just can't remember your old one."
print("\nOriginal string:")
print(text)
result = WordPunctTokenizer().tokenize(text)
print("\nSplit all punctuation into separate tokens:")
print(result)


# 5. Write a Python NLTK program to tokenize words, sentence wise.
text = "Joe waited for the train. The train was late. Mary and Samantha took the bus. I looked for Mary and Samantha at the bus station."
print("\nOriginal string:")
print(text)
print("\nTokenize words sentence wise:")
result = [word_tokenize(t) for t in sent_tokenize(text)]
print("\nRead the list:")
for s in result:
    print(s)


# 6. Write a Python NLTK program to tokenize a twitter text.
tknzr = TweetTokenizer(strip_handles=True, reduce_len=True)
tweet_text = "NoSQL introduction - w3resource http://bit.ly/1ngHC5F  #nosql #database #webdev"
print("\nOriginal Tweet:")
print(tweet_text)
result = tknzr.tokenize(tweet_text)
print("\nTokenize a twitter text:")
print(result)


# 7. Write a Python NLTK program to remove Twitter username handles from a given twitter text.
tknzr = TweetTokenizer(strip_handles=True)
tweet_text = "@abcd @pqrs NoSQL introduction - w3resource http://bit.ly/1ngHC5F  #nosql #database #webdev"
print("\nOriginal Tweet:")
print(tweet_text)
result = tknzr.tokenize(tweet_text)
print("\nTokenize a twitter text:")
print(result)


# 8. Write a Python NLTK program that will read a given text through each line and look for sentences. Print each sentence and divide two sentences with "==============".
text = '''
Mr. Smith waited for the train. The train was late.
Mary and Samantha took the bus. I looked for Mary and
Samantha at the bus station.
'''
print("\nOriginal Tweet:")
print(text)
sent_detector = nltk.data.load('tokenizers/punkt/english.pickle')
print('\n==============\n'.join(sent_detector.tokenize(text.strip())))


# 9. Write a Python NLTK program to find parenthesized expressions in a given string and divides the string into a sequence of substrings.
text = '(a b (c d)) e f (g)'
print("\nOriginal Tweet:")
print(text)
print(SExprTokenizer().tokenize(text))
text = '(a b) (c d) e (f g)'
print("\nOriginal Tweet:")
print(text)
print(SExprTokenizer().tokenize(text))
text = '[(a b (c d)) e f (g)]'
print("\nOriginal Tweet:")
print(text)
print(SExprTokenizer().tokenize(text))
print(text)
print(SExprTokenizer().tokenize(text))
text = '{a b {c d}} e f {g}'
print("\nOriginal Tweet:")
print(text)
print(SExprTokenizer().tokenize(text))




NumpyArray.py """
1. Write a Python program to print the NumPy version in your system.
"""
import timeit
import numpy as np
print(np.__version__)

"""
2. Write a Python program to convert a list of numeric value into a
one-dimensional NumPy array. 
Expected Output:
Original List: [12.23, 13.32, 100, 36.32] 
One-dimensional numpy array: [ 12.23 13.32 100. 36.32]
"""
my_arr = np.array([12.23, 13.32, 100, 36.32])
print(my_arr)

"""
3. Create a 3x3 matrix with values ranging from 2 to 10. 
Expected Output:
[[ 2 3 4] 
[ 5 6 7] 
[ 8 9 10]]
"""
my_arr = np.arange(2, 11).reshape(3, 3)
print(my_arr)

"""
4. Write a Python program to create a null vector of size 10 and update sixth
value to 11.
[ 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.] 
Update sixth value to 11 
[ 0. 0. 0. 0. 0. 0. 11. 0. 0. 0.]
"""
my_arr = np.zeros(10)
print(my_arr)
my_arr[6] = 11
print(my_arr)

"""
5. Write a Python program to create a array with values ranging from 12 to 38.
Expected Output:
[12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37]
"""
my_arr = np.arange(12, 39)
print(my_arr)

"""
6. Write a Python program to reverse an array (first element becomes last). 
Original array: 
[12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37] 
Reverse array: 
[37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12]
"""

my_arr = np.arange(12, 38)
print(my_arr[::-1])

"""
7. Write a Python program to an array converted to a float type. 
Sample output:
Original array 
[1, 2, 3, 4] 
Array converted to a float type: 
[ 1. 2. 3. 4.]
"""
my_arr = np.array([1, 2, 3, 4], dtype=float)
print(my_arr)
my_arr = np.asfarray([1, 2, 3, 4])
print(my_arr)

"""
8. Write a Python program to create a 2d array with 1 on the border and 0 inside. 
Expected Output:
Original array: 
[[ 1. 1. 1. 1. 1.] 
[ 1. 1. 1. 1. 1.] 
[ 1. 1. 1. 1. 1.] 
[ 1. 1. 1. 1. 1.] 
[ 1. 1. 1. 1. 1.]] 
1 on the border and 0 inside in the array 
[[ 1. 1. 1. 1. 1.] 
[ 1. 0. 0. 0. 1.] 
[ 1. 0. 0. 0. 1.] 
[ 1. 0. 0. 0. 1.] 
[ 1. 1. 1. 1. 1.]]
"""
my_arr = np.ones((5, 5))
print(my_arr)
my_arr[1:4, 1:4] = 0
print(my_arr)
"""
9. Write a Python program to add a border (filled with 0's) around an existing
array.
Expected Output:
Original array: 
[[ 1. 1. 1.] 
[ 1. 1. 1.] 
[ 1. 1. 1.]] 
1 on the border and 0 inside in the array 
[[ 0. 0. 0. 0. 0.] 
[ 0. 1. 1. 1. 0.] 
[ 0. 1. 1. 1. 0.] 
[ 0. 1. 1. 1. 0.] 
[ 0. 0. 0. 0. 0.]]
"""
my_arr = np.ones((3, 3))
my_arr = np.pad(my_arr, 1, 'constant', constant_values=0)
print(my_arr)

"""
10. Write a Python program to create a 8x8 matrix and fill it with a
checkerboard pattern. 
Checkerboard pattern:
[[0 1 0 1 0 1 0 1] 
[1 0 1 0 1 0 1 0] 
[0 1 0 1 0 1 0 1] 
[1 0 1 0 1 0 1 0] 
[0 1 0 1 0 1 0 1] 
[1 0 1 0 1 0 1 0] 
[0 1 0 1 0 1 0 1] 
[1 0 1 0 1 0 1 0]]
"""

my_arr = np.zeros((8, 8))
my_arr[::2, 1::2] = 1
my_arr[1::2, ::2] = 1
print(my_arr)

"""
11. Write a Python program to convert a list and tuple into arrays. 
List to array:
[1 2 3 4 5 6 7 8] 
Tuple to array:
[[8 4 6] 
[1 2 3]]
"""
my_arr = np.array([1, 2, 3, 4, 5, 6, 7, 8])
print(my_arr)
my_arr2 = np.array([(8, 4, 6), (1, 2, 3)])
print(my_arr2)

"""
12. Write a Python program to append values to the end of an array. 
Expected Output:
Original array:
[10, 20, 30] 
After append values to the end of the array:
[10 20 30 40 50 60 70 80 90]
"""

my_arr = np.array([10, 20, 30])
new_arr = np.append(my_arr, [40, 50, 60, 70, 80, 90])
print(new_arr)

"""
13. Write a Python program to create an empty and a full array. 
Expected Output:
[ 6.93270651e-310 1.59262180e-316 6.93270559e-310 6.93270665e-310]
[ 6.93270667e-310 6.93270671e-310 6.93270668e-310 6.93270483e-310] 
[ 6.93270668e-310 6.93270671e-310 6.93270370e-310 6.93270488e-310]] 
[[6 6 6] 
[6 6 6] 
[6 6 6]]
"""


my_arr = np.empty((3, 3))
print(my_arr)
my_arr = np.array([6]*9).reshape((3, 3))
print(my_arr)
my_arr = np.full((3, 3), 6)
print(my_arr)

"""
14. Write a Python program to convert the values of Centigrade degrees into
Fahrenheit degrees. Centigrade values are stored into a NumPy array. 
Sample Array [0, 12, 45.21 ,34, 99.91]
Expected Output:
Values in Fahrenheit degrees:
[ 0. 12. 45.21 34. 99.91] 
Values in Centigrade degrees: 
[-17.77777778 -11.11111111 7.33888889 1.11111111 37.72777778]
"""


celsius = np.array([0, 12, 45.21, 34, 99.91])
print(celsius)
fahrenheit = celsius*9/5 + 32
print(fahrenheit)

"""
16. Write a Python program to find the number of elements of an array, length of
one array element in bytes and total bytes consumed by the elements. 
Expected Output:
Size of the array: 3
Length of one array element in bytes: 8 
Total bytes consumed by the elements of the array: 24 
"""

my_arr = np.full((3, 3), 5)
print(my_arr)
print('Size of the array (number of elements):', my_arr.size)
print('Length of one array element in bytes:', my_arr.itemsize)
print('Total bytes consumed by the elements of the array:', my_arr.nbytes)

"""
17. Write a Python program to test whether each element of a 1-D array is also
present in a second array. 
Expected Output:
Array1: [ 0 10 20 40 60]
Array2: [0, 40] 
Compare each element of array1 and array2 
[ True False False True False]
"""

my_arr = np.array([0, 10, 20, 40, 60])
my_arr2 = np.array([0, 40])

compare = np.in1d(my_arr, my_arr2)
print(compare)

"""
18. Write a Python program to find common values between two arrays.
Expected Output:
Array1: [ 0 10 20 40 60] 
Array2: [10, 30, 40]
Common values between two arrays:
[10 40]
"""

my_arr = np.array([0, 10, 20, 40, 60])
my_arr2 = np.array([10, 30, 40])

compare = np.intersect1d(my_arr, my_arr2)
print(compare)

"""
19. Write a Python program to get the unique elements of an array. 
Expected Output:
Original array:
[10 10 20 20 30 30] 
Unique elements of the above array:
[10 20 30] 
Original array:
[[1 1] 
[2 3]]
Unique elements of the above array:
[1 2 3]
"""


my_arr = np.array([10, 10, 20, 20, 30, 30])
print(np.unique(my_arr))
my_arr = np.array([[1, 1], [2, 3]])
print(np.unique(my_arr))

"""
20. Write a Python program to find the set difference of two arrays. The set
difference will return the sorted, unique values in array1 that are not in array2.
Expected Output:
Array1: [ 0 10 20 40 60 80]
Array2: [10, 30, 40, 50, 70, 90] 
Set difference between two arrays:
[ 0 20 60 80]
"""


my_arr = np.array([0, 10, 20, 40, 60, 80])
my_arr2 = np.array([10, 30, 40, 50, 70, 90])

print(np.setdiff1d(my_arr, my_arr2))

"""
21. Write a Python program to find the set exclusive-or of two arrays. Set
exclusive-or will return the sorted, unique values that are in only one
(not both) of the input arrays.
Array1: [ 0 10 20 40 60 80] 
Array2: [10, 30, 40, 50, 70] 
Unique values that are in only one (not both) of the input arrays: 
[ 0 20 30 50 60 70 80]
"""


my_arr = np.array([0, 10, 20, 40, 60, 80])
my_arr2 = np.array([10, 30, 40, 50, 70, 90])

print(np.setxor1d(my_arr, my_arr2))

"""
22. Write a Python program to find the union of two arrays. Union will return the unique, sorted array of values that are in either of the two input arrays. Go to the editor
Array1: [ 0 10 20 40 60 80]
Array2: [10, 30, 40, 50, 70]
Unique sorted array of values that are in either of the two input arrays:
[ 0 10 20 30 40 50 60 70 80]
"""

my_arr = np.array([0, 10, 20, 40, 60, 80])
my_arr2 = np.array([10, 30, 40, 50, 70, 90])

print(np.union1d(my_arr, my_arr2))

"""
23. Write a Python program to test if all elements in an array evaluate to True.
Note: 0 evaluates to False in python.
"""

my_arr = np.array([0, 10, 20, 40, 60, 80])
my_arr2 = np.array([10, 30, 40, 50, 70, 90])

print(np.all(my_arr))
print(np.all(my_arr2))

"""
24. Write a Python program to test whether any array element along a given axis
evaluates to True. 
Note: 0 evaluates to False in python.
"""

my_arr = np.array([0, 10, 20, 40, 60, 80])
my_arr2 = np.array([10, 30, 40, 50, 70, 90])

print(np.any(my_arr))
print(np.any(my_arr2))

"""
25. Write a Python program to construct an array by repeating. 
Sample array: [1, 2, 3, 4]
Expected Output:
Original array 
[1, 2, 3, 4] 
Repeating 2 times 
[1 2 3 4 1 2 3 4]
Repeating 3 times 
[1 2 3 4 1 2 3 4 1 2 3 4]
"""


my_arr = np.array([1, 2, 3, 4])

# solution 1
print(np.concatenate((my_arr, my_arr)))
print(np.concatenate((my_arr, my_arr, my_arr)))
# solution 2
print(np.tile(my_arr, 2))
print(np.tile(my_arr, 3))

"""
26. Write a Python program to repeat elements of an array.
Expected Output:
[3 3 3 3] 
[1 1 2 2 3 3 4 4]
"""

print(np.repeat(3, 4))
print(np.repeat([1, 2, 3, 4], 2))

"""
27. Write a Python program to find the indices of the maximum and minimum values
along the given axis of an array. 
Original array: [1 2 3 4 5 6] 
Maximum Values: 5 
Minimum Values: 0
"""

my_arr = np.array([1, 2, 3, 4, 5])
print('Maximum Values:', np.argmax(my_arr))
print('Maximum Values:', np.argmin(my_arr))

"""
28. Write a Python program compare two arrays using numpy. 
Array a: [1 2]
Array b: [4 5]
a > b 
[False False]
a >= b 
[False False] 
a < b 
[ True True] 
a <= b 
[ True True]
"""

arr_a = np.array([1, 2])
arr_b = np.array([4, 2])
print(np.greater(arr_a, arr_b))
print(np.greater_equal(arr_a, arr_b))
print(np.less(arr_a, arr_b))
print(np.less_equal(arr_a, arr_b))

"""
29. Write a Python program to sort along the first, last axis of an array.
Sample array: [[4,6],[2,1]]
Expected Output:
Original array:
[[4 6] 
[2 1]] 
Sort along the first axis:
[[2 1] 
[4 6]] 
Sort along the last axis:
[[1 2] 
[4 6]]
"""

my_arr = np.array([[4, 6], [2, 1]])
print('Original array:\n', my_arr)
my_arr.sort(axis=0)
print('Sort along the first axis:\n', my_arr)
my_arr.sort()
print('Sort along the last axis:\n', my_arr)

"""
30. Write a Python program to sort pairs of first name and last name return their
indices. (first by last name, then by first name). 
first_names = (Betsey, Shelley, Lanell, Genesis, Margery)
last_names = (Battle, Brien, Plotner, Stahl, Woolum)
Expected Output:
[0 1 2 3 4]
[0 3 2 4 1]
"""

first_names = ('Betsey', 'Shelley', 'Lanell', 'Genesis', 'Margery')
surnames = ('Battle', 'Brien', 'Plotner', 'Stahl', 'Woolum')
ind = np.lexsort((first_names, surnames))
print('by last name:\n', ind)
ind = np.lexsort((surnames, first_names))
print('by first name:\n', ind)

"""
31. Write a Python program to get the values and indices of the elements that are
bigger than 10 in a given array. 
Original array:
[[ 0 10 20] 
[20 30 40]]
Values bigger than 10 = [20 20 30 40]
Their indices are (array([0, 1, 1, 1]), array([2, 0, 1, 2]))
"""

my_arr = np.array([0, 10, 20, 20, 30, 40]).reshape((2, 3))
print('original array:\n', my_arr)
# solution 1
print(my_arr[my_arr > 10])
print(np.nonzero(my_arr > 10))
# solution 2
print(my_arr[np.where(my_arr > 10)])
print(np.nonzero(my_arr > 10))


"""
32. Write a Python program to save a NumPy array to a text file.
"""

my_arr = np.array([[2, 3, 4], [5, 6, 7]])
np.savetxt('myarray.txt', my_arr, fmt='%.2f', delimiter=',')

"""
33. Write a Python program to find the memory size of a NumPy array.
Expected Output:
128 bytes
"""

my_arr = np.array([[2, 3, 4], [5, 6, 7]])
# solution 1
print(my_arr.nbytes)
# solution 2
print(my_arr.size * my_arr.itemsize)
# solution 3
print(np.prod(my_arr.shape) * my_arr.itemsize)

"""
34. Write a Python program to create an array of ones and an array of zeros.
Expected Output:
Create an array of zeros
Default type is float
[[ 0. 0.]] 
Type changes to int
[[0 0]] 
Create an array of ones
Default type is float 
[[ 1. 1.]] 
Type changes to int
[[1 1]]
"""

print(np.zeros((3, 3), dtype=int))
print(np.ones((3, 3), dtype=int))
print(np.zeros((3, 3)))
print(np.ones((3, 3)))

"""
35. Write a Python program to change the dimension of an array. 
Expected Output:
6 rows and 0 columns
(6,)
(3, 3) -> 3 rows and 3 columns
[[1 2 3] 
[4 5 6] 
[7 8 9]]
Change array shape to (3, 3) -> 3 rows and 3 columns 
[[1 2 3] 
[4 5 6] 
[7 8 9]]
"""


my_arr = np.ones((9,))
print(my_arr)
print(my_arr.shape)
my_arr = my_arr.reshape((3, 3))
print(my_arr)
print(my_arr.shape)

"""
36. Write a Python program to create a contiguous flattened array.
Original array:
[[10 20 30] 
[20 40 50]] 
New flattened array: 
[10 20 30 20 40 50]
"""

my_arr = np.array([[10, 20, 30], [20, 40, 50]])
print(my_arr, '\n', my_arr.shape)
print(np.ravel(my_arr), '\n', np.ravel(my_arr).shape)

"""
37. Write a Python program to create a 2-dimensional array of size 2 x 3
(composed of 4-byte integer elements), also print the shape, type and data type
of the array. 
Expected Output:
(2, 3)
int32
"""

my_arr = np.ones((2, 3), dtype=np.int32)
print(my_arr)
print(my_arr.shape)
print(my_arr.dtype)
print(my_arr.itemsize)
print(type(my_arr))

"""
38. Write a Python program to create a new shape to an array without changing its
data. 
Reshape 3x2: 
[[1 2] 
[3 4] 
[5 6]] 
Reshape 2x3:
[[1 2 3] 
[4 5 6]]
"""

my_arr = np.array(([1, 2], [3, 4], [5, 6]))
print(my_arr)
my_arr.shape = (2, 3)
print(my_arr)

"""
39. Write a Python program to change the data type of an array.
Expected Output:
[[ 2 4 6]
[ 6 8 10]] 
Data type of the array x is: int32 
New Type: float64 
[[ 2. 4. 6.] 
[ 6. 8. 10.]]
"""

my_arr = np.array([[2, 4, 6], [6, 8, 10]])
print(my_arr, '\n', my_arr.dtype)
new_arr = my_arr.astype('float64')
print(new_arr, '\n', new_arr.dtype)

"""
40. Write a Python program to create a new array of 3*5, filled with 2. Go to the editor
Expected Output:
[[2 2 2 2 2]
[2 2 2 2 2]
[2 2 2 2 2]]
"""

my_arr = np.full((3, 5), 2)
print(my_arr)

"""
41. Write a Python program to create an array of 10's with the same shape and
type of an given array. 
Sample array: x = np.arange(4, dtype=np.int64)
Expected Output:
[10 10 10 10]
"""

x = np.arange(4, dtype=np.int64)
print(x)
y = np.full_like(x, 10)
print(y)

"""
42. Write a Python program to create a 3-D array with ones on the diagonal and
zeros elsewhere. 
Expected Output:
[[ 1. 0. 0.]
[ 0. 1. 0.] 
[ 0. 0. 1.]]
"""

my_arr = np.eye(3)
print(my_arr)
my_arr = np.identity(3)
print(my_arr)

"""
43. Write a Python program to create a 2-D array whose diagonal equals
[4, 5, 6, 8] and 0's elsewhere. 
Expected Output:
[[4 0 0 0] 
[0 5 0 0] 
[0 0 6 0] 
[0 0 0 8]]
"""

my_arr = np.diag([4, 5, 6, 8])
print(my_arr)

"""
44. Write a Python program to create a 1-D array going from 0 to 50 and an array
from 10 to 50. 
Expected Output:
Array from 0 to 50: 
[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 
25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49] 
Array from 10 to 50: 
[10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 
35 36 37 38 39 40 41 42 43 44 45 46 47 48 49]
"""

my_arr = np.arange(50, dtype=int)
my_arr2 = np.arange(10, 51, dtype=int)
print(my_arr)
print(my_arr2)

"""
45. Write a Python program to Create a 1-D array of 30 evenly spaced elements
between 2.5. and 6.5, inclusive. Go to the editor
Expected Output:
[ 2.5 2.63793103 2.77586207 2.9137931 3.05172414 3.18965517 
3.32758621 3.46551724 3.60344828 3.74137931 3.87931034 4.01724138 
4.15517241 4.29310345 4.43103448 4.56896552 4.70689655 4.84482759
4.98275862 5.12068966 5.25862069 5.39655172 5.53448276 5.67241379
5.81034483 5.94827586 6.0862069 6.22413793 6.36206897 6.5 ]
"""

my_arr = np.linspace(2.5, 6.5, num=30, endpoint=True)
print(my_arr)

"""
46. Write a Python program to to create a 1-D array of 20 element spaced evenly
on a log scale between 2. and 5., exclusive. 
Expected Output:
[ 100. 141.25375446 199.5262315 281.83829313
398.10717055 562.34132519 794.32823472 1122.0184543 
1584.89319246 2238.72113857 3162.27766017 4466.83592151
6309.5734448 8912.50938134 12589.25411794 17782.79410039
25118.8643151 35481.33892336 50118.72336273 70794.57843841]
"""
my_arr = np.logspace(2., 5., num=20, endpoint=False)
print(my_arr)

"""
47. Write a Python program to create an array which looks like below array.
Expected Output:
[[ 0. 0. 0.]
[ 1. 0. 0.]
[ 1. 1. 0.]
[ 1. 1. 1.]]
"""
my_arr = np.tri(4, 3, -1)
print(my_arr)

"""
48. Write a Python program to create an array which looks like below array.
Expected Output:
[[ 2 3 4] 
[ 5 6 7] 
[ 0 9 10] 
[ 0 0 13]]
"""

my_arr = np.arange(2, 14).reshape(4, 3)
print(np.triu(my_arr, -1))

"""
49. Write a Python program to collapse a 3-D array into one dimension array.
Expected Output:
3-D array: 
[[ 1. 0. 0.] 
[ 0. 1. 0.] 
[ 0. 0. 1.]]
One dimension array: 
[ 1. 0. 0. 0. 1. 0. 0. 0. 1.]
"""

my_arr = np.eye(3).ravel()
print(my_arr)

"""
50. Write a Python program to find the 4th element of a specified array.
Expected Output:
[[ 2 4 6]
[ 6 8 10]]
Forth e1ement of the array:
6
"""

my_arr = np.array([[2, 4, 6], [6, 8, 10]])
print(my_arr.flat[3])

"""
51. Write a Python program to interchange two axes of an array. 
Sample array: [[1 2 3]] 
Expected Output:
[[1] 
[2] 
[3]]
"""
my_arr = np.array([[1, 2, 3]])
print(np.swapaxes(my_arr, 0, 1))

"""
52. Write a Python program to move axes of an array to new positions. Other axes
remain in their original order.
Expected Output:
(3, 4, 2) 
(4, 2, 3)
"""

my_arr = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
print('original:\n', my_arr)
print('transposed:')
# solution 1
print(np.moveaxis(my_arr, 0, 1))
# solution 2
print(np.transpose(my_arr))
# solution 3
print(np.swapaxes(my_arr, 0, 1))
# solution 4
print(np.moveaxis(my_arr, [0, 1], [-1, -2]))

"""
53. Write a Python program to move the specified axis backwards, until it lies
in a given position.
Move the following 3rd array axes to first position.
(2,3,4,5)
Sample Expected Output:
(2, 5, 3, 4)
"""
# This function continues to be supported for backward compatibility,
# but you should prefer moveaxis. The moveaxis function was added in NumPy 1.11.

my_arr = np.zeros((2, 3, 4, 5))
print(my_arr.shape)
print(np.rollaxis(my_arr, 3, 1).shape)
print(np.moveaxis(my_arr, 3, 1).shape)

"""
54. Write a Python program to convert specified inputs to arrays with at least one
dimension.
Expected Output:
[ 12.] 
[[ 0. 1. 2.]
[ 3. 4. 5.]]
[array([1]), array([3, 4])]
"""

x = 12.
print(np.atleast_1d(x))
x = [[0., 1., 2.], [3., 4., 5.]]
print(np.atleast_1d(x))
x = 1
y = [3, 4]
print(np.atleast_1d(x, y))

"""
55. Write a Python program to view inputs as arrays with at least two dimensions,
three dimensions. 
Expected Output:
View inputs as arrays with at least two dimensions:
[10] 
[[ 0. 1.]
[ 2. 3.]] 
View inputs as arrays with at least three dimensions:
[[[15]]] 
[[[ 0.] 
[ 1.] 
[ 2.]]]
"""


x = 10
print(np.atleast_2d(x))
x = [[0., 1.], [2., 3.]]
print(np.atleast_2d(x))
x = 15
print(np.atleast_3d(x))
x = [0., 1., 2.]
print(np.atleast_3d(x))

"""
56. Write a Python program to insert a new axis within a 2-D array. 
2-D array of shape (3, 4).
Expected Output:
New shape will be will be (3, 1, 4).
"""

x = np.arange(12).reshape((3, 4))
print(x)
print(np.expand_dims(x, 1).shape)

"""
57. Write a Python program to remove single-dimensional entries from a specified
shape.
Specified shape: (3, 1, 4)
Expected Output: (3, 4)
"""

x = np.zeros(12).reshape((3, 1, 4))
print(x)
print(np.squeeze(x))
print(np.squeeze(x).shape)

"""
58. Write a Python program to concatenate two 2-dimensional arrays. 
Expected Output:
Sample arrays: [[0, 1, 3], [5, 7, 9]], [[0, 2, 4], [6, 8, 10]]
Expected Output:
[[ 0 1 3 0 2 4]
[ 5 7 9 6 8 10]]
"""

x = np.array([[0, 1, 3], [5, 7, 9]])
y = np.array([[0, 2, 4], [6, 8, 10]])
print(np.concatenate((x, y), 1))

"""
59. Write a Python program to convert 1-D arrays as columns into a 2-D array.
Sample array: (10,20,30), (40,50,60)
Expected Output:
[[10 40]
[20 50]
[30 60]]
"""

x = np.array((10, 20, 30))
y = np.array((40, 50, 60))
print(np.column_stack((x, y)))

"""
60. Write a Python program to convert (in sequence depth wise (along third axis)) two 1-D arrays into a 2-D array. Go to the editor
Sample array: (10,20,30), (40,50,60)
Expected Output:
[[[10 40]]
[[20 50]]
[[30 60]]]
"""

x = np.array((10, 20, 30))
y = np.array((40, 50, 60))
print(np.dstack((x, y)))

"""
61. Write a Python program to split an array of 14 elements into 3 arrays, each of
which has 2, 4, and 8 elements in the original order. 
Expected Output:
Original array: [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14]
After splitting: 
[array([1, 2]), array([3, 4, 5, 6]), array([ 7, 8, 9, 10, 11, 12, 13, 14])]
"""

x = np.arange(1, 15)
print(x)
print(np.array_split(x, [2, 6]))
print(np.split(x, [2, 6]))

"""
62. Write a Python program to split an array of shape 4x4 it into two arrays
along the second axis. 
Sample array :
[[ 0 1 2 3]
[ 4 5 6 7] 
[ 8 9 10 11] 
[12 13 14 15]]
Expected Output:
[array([[ 0, 1],
[ 4, 5], 
[ 8, 9], 
[12, 13]]), array([[ 2, 3],
[ 6, 7],
[10, 11], 
[14, 15]]), array([], shape=(4, 0), dtype=int64)]
"""

x = np.arange(16).reshape(4, 4)
print(x)
print(np.split(x, 2, axis=-1))
print(np.hsplit(x, 2))

"""
63. Write a Python program to get the number of nonzero elements in an array.
Expected Output:
Original array: 
[[ 0 10 20] 
[20 30 40]]
Number of non zero elements in the above array: 
5
"""

x = np.array([[0, 10, 20], [20, 30, 40]])
print(np.count_nonzero(x))

"""
64. Write a Python program to create a 5x5 matrix with row values ranging from
0 to 4. 
Original array:
[[ 0. 0. 0. 0. 0.]
[ 0. 0. 0. 0. 0.]
[ 0. 0. 0. 0. 0.]
[ 0. 0. 0. 0. 0.]
[ 0. 0. 0. 0. 0.]] 
Row values ranging from 0 to 4.
[[ 0. 1. 2. 3. 4.]
[ 0. 1. 2. 3. 4.] 
[ 0. 1. 2. 3. 4.]
[ 0. 1. 2. 3. 4.]
[ 0. 1. 2. 3. 4.]]
"""

x = np.zeros((5, 5))
print(x)
x += np.arange(5)
print(x)

"""
65. Write a Python program to test if specified values are present in an array.
Expected Output:
Original array:
[[ 1.12 2. 3.45] 
[ 2.33 5.12 6. ]] 
True 
False 
True 
False 
True
"""

x = np.array([[1.12, 2.0, 3.45], [2.33, 5.12, 6.0]])
print(1.12 in x)
print(1.13 in x)

"""
66. Write a Python program to create a vector of size 10 with values ranging from
0 to 1, both excluded. 
Expected Output:
[ 0.09090909 0.18181818 0.27272727 0.36363636 0.45454545 0.54545455 
0.63636364 0.72727273 0.81818182 0.90909091]
"""

x = np.linspace(0.09, 1, num=10, endpoint=False)
print(x)

"""
67. Write a Python program to make an array immutable (read-only).
Expected Output:
Test the array is read-only or not:
Try to change the value of the first element:
ValueError: assignment destination is read-only
"""

x = np.arange(5)
print(x)
print(np.delete(x, 2))
x.flags.writeable = False
x[2] = 2

"""
68. Write a Python program (using numpy) to sum of all the multiples of 3 or 5
below 100. 
Expected Output:
[ 3 5 6 9 10 12 15 18 20 21 24 25 27 30 33 35 36 39 40 42 45 48 50 51 54
55 57 60 63 65 66 69 70 72 75 78 80 81 84 85 87 90 93 95 96 99]
2318
"""

x = np.arange(100)
y = x[np.where((x % 5 == 0) | (x % 3 == 0))]
print(y)
print(y.sum())

"""
69. Write a Python program to create an array with 10^3 elements.
Expected Output:
[ 0. 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11.
12. 13. 14. 15. 16. 17. 18. 19. 20. 21. 22. 23. 
24. 25. 26. 27. 28. 29. 30. 31. 32. 33. 34. 35. 
- - - - - - - - - - - - - - - - - - - -
972. 973. 974. 975. 976. 977. 978. 979. 980. 981. 982. 983.
984. 985. 986. 987. 988. 989. 990. 991. 992. 993. 994. 995. 
996. 997. 998. 999.]
"""

print(np.arange(1000.))

"""
70. Write a Python program to create display every element of an numpy array.
Expected Output:
0 1 2 3 4 5 6 7 8 9 10 11
"""

x = np.arange(12)

for i in x.flatten():
    print(i, end=' ')
print('\n')

"""
71. Write a Python program to create and display every element of an numpy array
in Fortran order. 
Expected Output:
Elements of the array in Fortan array: 
0 4 8 1 5 9 2 6 10 3 7 11
"""

x = np.arange(12)

for i in x.flatten(order='F'):
    print(i, end=' ')
print('\n')

"""
72. Write a Python program to create a 5x5x5 cube of 1's. 
Expected Output:
[[[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1]] 

[[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1]] 

[[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1]] 

[[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1]] 

[[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1] 
[1 1 1 1 1]]]
"""

print(np.ones((5, 5, 5)))

"""
73. Write a Python program to create an array of (3, 4) shape, multiply every
element value by 3 and display the new array. 
Expected Output:
Original array elements: 
[[ 0 1 2 3] 
[ 4 5 6 7] 
[ 8 9 10 11]] 
New array elements:
[[ 0 3 6 9] 
[12 15 18 21] 
[24 27 30 33]]
"""


x = np.arange(12).reshape(3, 4)
y = x*3
print(x)
print(y)

"""
74. Write a Python program to combine a one and a two dimensional array together
and display their elements. 
Expected Output:
One dimensional array:
[0 1 2 3] 
Two dimensional array: 
[[0 1 2 3] 
[4 5 6 7]] 
0:0 
1:1 
2:2 
3:3
0:4
1:5
2:6
3:7
"""

x = np.array([0, 1, 2, 3])
y = np.array([[0, 1, 2, 3], [4, 5, 6, 7]])
print(x)
print(x.shape)
print(y)
print(y.shape)
for x, y in np.nditer([x, y]):
    print(x, ':', y)

"""
75. Write a Python program to create an array of zeros and three column types
(integer, float, character). 
Expected Output:
[(1, 2., b'Albert Einstein') (2, 2., b'Edmond Halley')
(3, 3., b'Gertrude B. Elion')]
"""

x = np.zeros((3,), dtype=('i4,f8,U25'))
print(x)
x[...] = [(1, 2., b'Albert Einstein'), (2, 2., b'Edmond Halley'),
          (3, 3., b'Gertrude B. Elion')]
print(x)

"""
76. Write a Python program to create a function cube which cubes all the elements
of an array. 
Expected Output:
[ 1 8 27]
"""


# my solution
x = np.arange(1, 4)
print(x)
for i in np.nditer(x, op_flags=['readwrite']):
    i[...] = i**3
print(x)

# solution from scipy docs


def cube(a):
    it = np.nditer([a, None])
    for x, y in it:
        y[...] = x*x*x
    return it.operands[1]


print(cube([1, 2, 3]))

# short solution
x = np.arange(1, 4)
print(x*x*x)

"""
77. Write a Python program to create an array of (3, 4) shape and convert the
array elements in smaller chunks. 
Expected Output:
Original array elements:
[[ 0 1 2 3] 
[ 4 5 6 7]
[ 8 9 10 11]]
[0 4 8] 
[1 5 9] 
[ 2 6 10]
[ 3 7 11]
"""

# solution with hsplit
x = np.arange(12).reshape(3, 4)
print(x)
y = np.hsplit(x, 4)
for ary in y:
    print(ary.T.squeeze())

# solution with nditer
x = np.arange(12).reshape(3, 4)
print(x)
for row in np.nditer(x, flags=['external_loop'], order='F'):
    print(row)

"""
78. Write a Python program to create a record array from a (flat) list of arrays.
Sample arrays: [1,2,3,4], ['Red', 'Green', 'White', 'Orange'], [12.20,15,20,40]
Expected Output:
(1, 'Red', 12.2)
(2, 'Green', 15.0)
(3, 'White', 20.0)
"""

# dtype=[('id', 'i4'), ('color', 'U10'), ('float', 'f8')]
x = np.core.records.fromarrays([[1, 2, 3, 4], ['Red', 'Green', 'White', 'Orange'],
                                [12.20, 15, 20, 40]], names=['id', 'color', 'number'])
print(x)
print(x.shape)
print(x.dtype)
print(x.id)
for i in x:
    print(i)

"""
80. Write a Python program to convert a NumPy array into Python list structure. 
Expected Output:
Original array elements:
[[0 1]
[2 3] 
[4 5]] 
Array to list: 
[[0, 1], [2, 3], [4, 5]]
"""

x = np.arange(6).reshape(3, 2)
print(x)
l = x.tolist()
print(l)

"""
81. Write a Python program to access an array by column. 
Expected Output:
Original array elements:
[[0 1] 
[2 3] 
[4 5]] 
Access an array by column:
First column:
[0 1] 
Second column: 
[2 3] 
Third column:
[4 5]
"""

x = np.arange(6).reshape(3, 2)
print(x)
for col in x:
    print(col)

"""
82. Write a Python program to convert a numpy array of float values to a numpy array of integer values. Go to the editor
Expected Output:
Original array elements:
[[ 12. 12.51]
[ 2.34 7.98] 
[ 25.23 36.5 ]]
Conver float values to intger values:
[[12 12] 
[ 2 7] 
[25 36]]
"""

x = np.array([[12., 12.51], [2.34, 7.98], [25.23, 36.5]])
print(x)
print(x.dtype)
y = x.astype(int)
print(y)
print(y.dtype)

"""
83. Write a Python program to display numpy array elements of floating values
with given precision.
Expected Output:
Original array elements:
[ 0.26153123 0.52760141 0.5718299 0.5927067 0.7831874 0.69746349 
0.35399976 0.99469633 0.0694458 0.54711478] 
Print array values with precision 3: 
[ 0.262 0.528 0.572 0.593 0.783 0.697 0.354 0.995 0.069 0.547]
"""

x = np.array([0.26153123, 0.52760141, 0.5718299, 0.5927067])
print(x)

np.set_printoptions(precision=3)
print(x)

np.set_printoptions(precision=8)
print(x)

"""
84. Write a Python program to suppresses the use of scientific notation for small
numbers in numpy array. 
Expected Output:
Original array elements:
[ 1.60000000e-10 1.60000000e+00 1.20000000e+03 2.35000000e-01] 
Print array values with precision 3: 
[ 0. 1.6 1200. 0.235]
"""

x = np.array([1.60000000e-10, 1.60000000e+00, 1.20000000e+03, 2.35000000e-01])
print(x)
np.set_printoptions(precision=3, suppress=True)
print(x)

"""
85. Write a Python program to create a numpy array of 10 integers from a generator.
Expected Output:
[0 1 2 3 4 5 6 7 8 9]
"""


def func(i):
    yield from range(i)


x = np.fromiter(func(10,), dtype=int)
print(x)

"""
86. Write a Python program to how to add an extra column to an numpy array.
Expected Output:
[[ 10 20 30 100]
[ 40 50 60 200]]
"""

x = np.arange(1, 7).reshape(2, 3)*10
print(x, '\n', x.shape)
y = np.array([100, 200]).reshape(2, 1)
print(y, '\n', y.shape)


# with hstack
z = np.hstack((x, y))
print(z, '\n', z.shape)

# with concatenate
z = np.concatenate((x, y), axis=1)
print(z, '\n', z.shape)

# with column_stack
z = np.column_stack((x, y))
print(z, '\n', z.shape)

# with append
z = np.append(x, y, axis=1)
print(z, '\n', z.shape)

# with insert
z = np.insert(x, [x.shape[1]], y, axis=1)
print(z, '\n', z.shape)

"""
88. Write a Python program to replace all elements of numpy array that are greater
than specified array. 
Expected Output:
Original array:
[[ 0.42436315 0.48558583 0.32924763] 
[ 0.7439979 0.58220701 0.38213418]
[ 0.5097581 0.34528799 0.1563123 ]]
Replace all elements of the said array with .5 which are greater than. 5
[[ 0.42436315 0.48558583 0.32924763] 
[ 0.5 0.5 0.38213418] 
[ 0.5 0.34528799 0.1563123 ]]
"""

x = np.array([[0.42436315, 0.48558583, 0.32924763],
              [0.7439979, 0.58220701, 0.38213418],
              [0.5097581, 0.34528799, 0.1563123]])

x[x > 0.5] = 0.5
print(x)

"""
89. Write a Python program to remove specific elements in a numpy array.
Expected Output:
Original array: 
[ 10 20 30 40 50 60 70 80 90 100]
Delete first, fourth and fifth elements:
[ 20 30 60 70 80 90 100]
"""

x = np.arange(10, 101, 10)
print(x)
x = np.delete(x, [0, 3, 4], None)
print(x)

"""
90. Write a Python program to remove the negative values in a numpy array with 0.
"""

x = np.arange(-5, 5).reshape(5, 2)
print(x)

x[x < 0] = 0
print(x)

"""
91. Write a Python program to remove all rows in a numpy array that contain
non-numeric values.
Expected Output:
Original array: 
[[ 1. 2. 3.] 
[ 4. 5. nan]
[ 7. 8. 9.] 
[ 1. 0. 1.]] 
Remove all non-numeric elements of the said array
[[ 1. 2. 3.] 
[ 7. 8. 9.] 
[ 1. 0. 1.]]
"""

arr = np.array([[1., 2., 3.],
                [4., 5., np.nan],
                [7., 8., 9.],
                [1., 0., 1.]])
arr = arr[np.all(np.isfinite(arr), axis=1)]
print(arr)

"""
92. Write a Python program to select indices satisfying multiple conditions
in a numpy array.
Sample array :
a = np.array([97, 101, 105, 111, 117])
b = np.array(['a','e','i','o','u'])
Note: Select the elements from the second array corresponding to elements in the
first array that are greater than 100 and less than 110
Expected Output:
Original arrays 
[ 97 101 105 111 117] 
['a' 'e' 'i' 'o' 'u'] 
Elements from the second array corresponding to elements in the first 
array that are greater than 100 and less than 110: 
['e' 'i']
"""

a = np.array([97, 101, 105, 111, 117])
b = np.array(['a', 'e', 'i', 'o', 'u'])

z = b[(a > 100) & (a < 110)]
print(z)

"""
93. Write a Python program to get the magnitude of a vector in numpy.
Expected Output:
Original array:
[1 2 3 4 5]
Magnitude of the vector:
7.4161984871
"""

x = np.arange(1, 6)
print(np.linalg.norm(x))

"""
94. Write a Python program to count the frequency of unique values in numpy array.
Expected Output:
Original array:
[10 10 20 10 20 20 20 30 30 50 40 40] 
Frequency of unique values of the said array:
[[10 20 30 40 50] 
[ 3 4 2 2 1]]
"""

x = np.array([10, 10, 20, 10, 20, 20, 20, 30, 30, 50, 40, 40])
uniques, count = np.unique(x, return_counts=True)
print(np.array([uniques, count]))

"""
95. Write a Python program to check whether the numpy array is empty or not.
"""
x = np.arange(6)
y = np.array([])
print(x)
print(x.size)
print(y)
print(y.size)

"""
96. Write a Python program to divide each row by a vector element.
Expected Output:
Original array:
[[20 20 20] 
[30 30 30] 
[40 40 40]]
Vector: 
[20 30 40]
[[ 1. 1. 1.]
[ 1. 1. 1.] 
[ 1. 1. 1.]]
"""

x = np.array([[20, 20, 20], [30, 30, 30], [40, 40, 40]])
y = np.array([20, 30, 40]).reshape(3, 1)
z = x/y
print(x)
print(y)
print(z)

"""
97. Write a Python program to print all the values of an array.
Expected Output:
[[ 0. 0. 0. 0.]
[ 0. 0. 0. 0.]
[ 0. 0. 0. 0.]
[ 0. 0. 0. 0.]]
"""
x = np.zeros((4, 4))
print(x)

"""
98. Write a Python program to convert the raw data in an array to a binary string
and then create an array. 
Expected Output:
Original array:
[ 10. 20. 30.]
Binary string array:
b'\x00\x00\x00\x00\x00\x00$@\x00\x00\x00\x00\x00\x004@\x00\x00\x00\x00\x00\x00>@' 
Array using fromstring(): 
[ 10. 20. 30.]
"""

x = np.fromstring(b'\x00\x00\x00\x00\x00\x00$@\x00\x00\x00\x00\x00\x004@\x00\x00\x00\x00\x00\x00>@',
                  dtype=float, count=-1, sep='')
print(x)

"""
99. Write a Python program to sum and compute the product of a numpy array elements.
Expected Output:
Original array: 
[ 10. 20. 30.] 
Sum of the array elements:
60.0 
Product of the array elements: 
6000.0
"""

x = np.array([10, 20, 30.])
print(np.sum(x))
print(np.prod(x))

"""
100. Write a Python program to take values from a source array and put them at
specified indices of another array. 
Expected Output:
[ 10. 10. 20. 30. 30.] 
Put 0 and 40 in first and fifth position of the above array 
Array x after put two values: [ 0. 10. 20. 30. 40.] 
"""

# inserting
x = np.array([10, 10, 20, 30, 30])
x = np.insert(x, [0, 4], [0, 40])
print(x)

# replacing
x = np.array([10, 10, 20, 30, 30])
np.put(x, [0, 4], [0, 40])
print(x)




OperatingSystemServices.py import io
import glob
import time
import sys
import os
from os import stat_info

# 1. Write a Python program to get the name of the operating system(Platform independent), information of current operating system, current working directory, print files and directories in the current directory and raises error in the case of invalid or inaccessible file names and paths. 
print("Operating System:", os.name)
print("\nInformation of current operating system: ", os.uname())
print("\nCurrent Working Directory: ", os.getcwd())
print("\nList of files and directories in the current directory:")
print(os.listdir('.'))
print("\nTest if a specified file exis or not:")
try:
   filename = 'abc.txt'
   with open(filename, 'r') as f:
      text = f.read()
except IOError:
   print(f'Not accessed or problem in reading: {filename}')

# 2. Write a Python program to list only directories, files and all directories, files in a specified path. 
path = 'g:\\testpath\\'
print("Only directories:")
print([name for name in os.listdir(path)
       if os.path.isdir(os.path.join(path, name))])
print("\nOnly files:")
print([name for name in os.listdir(path)
       if not os.path.isdir(os.path.join(path, name))])
print("\nAll directories and files :")
print(list(os.listdir(path)))

# 3. Write a Python program to scan a specified directory and identify the sub directories and files.
root = 'g:\\testpath\\'
for entry in os.scandir(root):
   if entry.is_dir():
       typ = 'dir'
   elif entry.is_file():
       typ = 'file'
   elif entry.is_symlink():
       typ = 'link'
   else:
       typ = 'unknown'
   print('{name} {typ}'.format(
       name=entry.name,
       typ=typ,
   ))

# 4. Write a Python program to check for access to a specified path. Test the existence, readability, writability and executability of the specified path. 
print('Exist:', os.access('c:\\Users\\Public\\C programming library.docx', os.F_OK))
print('Readable:', os.access(
    'c:\\Users\\Public\\C programming library.docx', os.R_OK))
print('Writable:', os.access(
    'c:\\Users\\Public\\C programming library.docx', os.W_OK))
print('Executable:', os.access(
    'c:\\Users\\Public\\C programming library.docx', os.X_OK))

# 5. Write a Python program to get the size, permissions, owner, device, created, last modified and last accessed date time of a specified path.
path = 'g:\\testpath\\'
print(f'Path Name ({path}):')
print('Size:', stat_info.st_size)
print('Permissions:', oct(stat_info.st_mode))
print('Owner:', stat_info.st_uid)
print('Device:', stat_info.st_dev)
print('Created     :', time.ctime(stat_info.st_ctime))
print('Last modified:', time.ctime(stat_info.st_mtime))
print('Last accessed:', time.ctime(stat_info.st_atime))

# 6. Write a Python program to create a symbolic link and read it to decide the original file pointed by the link.
path = f'/tmp/{os.path.basename(__file__)}'
print(f'Creating link {path} -> {__file__}')
os.symlink(__file__, path)
stat_info = os.lstat(path)
print('\nFile Permissions:', oct(stat_info.st_mode))
print('\nPoints to:', os.readlink(path))
#removes the file path
os.unlink(path)

# 7. Write a Python program to create a file and write some text and rename the file name.
with open('a.txt', 'w') as f:
   f.write('Python program to create a symbolic link and read it to decide the original file pointed by the link.')
print('\nInitial file/dir name:', os.listdir())
with open('a.txt', 'r') as f:
   print('\nContents of a.txt:', repr(f.read()))
os.rename('a.txt', 'b.txt')
print('\nAfter renaming initial file/dir name:', os.listdir())
with open('b.txt', 'r') as f:
   print('\nContents of b.txt:', repr(f.read()))

# 8. Write a Python program to find the parent's process id, real user ID of the current process and change real user ID. 
print("Parent’s process id:", os.getppid())
uid = os.getuid()
print("\nUser ID of the current process:", uid)
uid = 1400
os.setuid(uid)
print("\nUser ID changed")
print("User ID of the current process:", os.getuid())

# 9. Write a Python program to retrieve the current working directory and change the dir(moving up one). 
print('Current dir:', os.getcwd())
print('\nChange the dir (moving up one):', os.pardir)
os.chdir(os.pardir)
print('Current dir:', os.getcwd())
print('\nChange the dir (moving up one):', os.pardir)
os.chdir(os.pardir)
print('Current dir:', os.getcwd())

# 10. Write a python program to access environment variables and value of the environment variable. 
import os
print("Access all environment variables:")
print('*----------------------------------*')
print(os.environ)
print('*----------------------------------*')
print("Access a particular environment variable:")
print(os.environ['HOME'])
print('*----------------------------------*')
print(os.environ['PATH'])
print('*----------------------------------*')
print('Value of the environment variable key:')
print(os.getenv('JAVA_HOME'))
print(os.getenv('PYTHONPATH'))

# 11. Write a Python program to iterate over a root level path and print all its sub-directories and files, also loop over specified dirs and files. 
print('Iterate over a root level path:')
path = '/tmp/'
for root, dirs, files in os.walk(path):
 print(root)

# 12. Write a Python program to test whether a given path exists or not. If the path exist find the filename and directory portion of the said path. 
print("Test a path exists or not:")
path = r'g:\\testpath\\a.txt'
print(os.path.exists(path))
path = r'g:\\testpath\\p.txt'
print(os.path.exists(path))
print("\nFile name of the path:")
print(os.path.basename(path))
print("\nDir name of the path:")
print(os.path.dirname(path))

# 13. Write a Python program to join one or more path components together and split a given path in directory and file. 
path = r'g:\\testpath\\a.txt'
print("Original path:")
print(path)
print("\nDir and file name of the said path:")
print(os.path.split(path))
print("\nJoin one or more path components together:")
print(os.path.join(r'g:\\testpath\\', 'a.txt'))

# 14. Write a Python program to alter the owner and the group id of a specified file. 
fd = os.open("/tmp", os.O_RDONLY)
os.fchown(fd, 100, -1)
os.fchown(fd, -1, 50)
print("Changed ownership successfully..")
os.close(fd)

# 15. Write a Python program to get information about the file pertaining to the file mode. Print the information - ID of device containing file, inode number, protection, number of hard links, user ID of owner, group ID of owner, total size (in bytes), time of last access, time of last modification and time of last status change. 
path = 'e:\\testpath\\p.txt'
fd = os.open(path, os.O_RDWR)
info = os.fstat(fd)
print(f"ID of device containing file: {info.st_dev}")
print(f"Inode number: {info.st_ino}")
print(f"Protection: {info.st_mode}")
print(f"Number of hard links: {info.st_nlink}")
print(f"User ID of owner: {info.st_uid}")
print(f"Group ID of owner: {info.st_gid}")
print(f"Total size, in bytes: {info.st_size}")
print(f"Time of last access: {info.st_atime}")
print(f"Time of last modification: {info.st_mtime }")
print(f"Time of last status change: {info.st_ctime }")
os.close(fd)

# 16. Write a Python program to write a string to a buffer and retrieve the value written, at the end discard buffer memory.
# Write a string to a buffer
output = io.StringIO()
output.write('Python Exercises, Practice, Solution')
# Retrieve the value written
print(output.getvalue())
# Discard buffer memory
output.close()

# 17. Write a Python program to run an operating system command using the os module.
command = "dir" if os.name == "nt" else "ls -l"
os.system(command)

# 18. Write a Python program to start a new process replacing the current process. 
program = "python"
arguments = ["hello.py"]
print(os.execvp(program, (program,) + tuple(arguments)))
print("Goodbye")

# 19. The epoch is the point where the time starts, and is platform dependent. For Unix, the epoch is January 1, 1970, 00: 00: 00 (UTC). Write a Python program to find out what the epoch is on a given platform. Also convert a given time in seconds since the epoch. 
print("\nEpoch on a given platform:")
print(time.gmtime(0))
print("\nTime in seconds since the epoch:")
print(time.gmtime(36000))

# 20. Write a Python program to get time values with components using local time and gmtime. 
def time_struct(s):
   print(' tm_year :', s.tm_year)
   print(' tm_mon :', s.tm_mon)
   print(' tm_mday :', s.tm_mday)
   print(' tm_hour :', s.tm_hour)
   print(' tm_min :', s.tm_min)
   print(' tm_sec :', s.tm_sec)
   print(' tm_wday :', s.tm_wday)
   print(' tm_yday :', s.tm_yday)
   print(' tm_isdst:', s.tm_isdst)

print('\nlocaltime:')
time_struct(time.localtime())
print('\ngmtime:')
time_struct(time.gmtime())

# 21. Write a Python program to get different time values with components timezone, timezone abbreviations, the offset of the local(non-DST) timezone, DST timezone and time of different timezones. 


def zone_info():
   print('TZ   :', os.environ.get('TZ', '(not set)'))
   print('Timezone abbreviations:', time.tzname)
   print(f'Timezone : {time.timezone} ({time.timezone / 3600})')
   print('DST timezone ', time.daylight)
   print('Time :', time.strftime('%X %x %Z'), '\n')


print('Default Zone:')
zone_info()
TIME_ZONES = [
    'Pacific/Auckland',
    'Europe/Berlin',
    'America/Detroit',
    'Singapore',
]
for zone in TIME_ZONES:
   os.environ['TZ'] = zone
   time.tzset()
   print(zone, ':')
   zone_info()

# 22. Write a Python program that can suspend execution of a given script a given number of seconds.
for _ in range(4):
   time.sleep(3)
   print("Sorry, Slept for 3 seconds...")

# 23. Write a Python program to convert a given time in seconds since the epoch to a string representing local time. 
print(time.ctime())
print(time.ctime(236543789))

# 24. Write a Python program to print simple format of time, full names and the representation format and preferred date time format. 
import time
print("\nSimple format of time:")
s = time.strftime("%a, %d %b %Y %H:%M:%S + 1010", time.gmtime())
print(s)
print("\nFull names and the representation:")
s = time.strftime("%A, %D %B %Y %H:%M:%S + 0000", time.gmtime())
print( s)
print("\nPreferred date time format:")
s = time.strftime("%c")
print(s)
s = time.strftime("%x, %X, %y, %Y")
print("Example 11:", s)

# 25. Write a Python program that takes a given number of seconds and pass since epoch as an argument. Print structure time in local time. 
result = time.localtime(414538698)
print("\nResult:", result)
print("\nYear:", result.tm_year)

# 26. Write a Python program that takes a tuple containing 9 elements corresponding to structure time as an argument and returns a string representing it. 
import time
t = (2020, 1, 22, 2, 34, 6, 6, 362, 0)
result = time.asctime(t)
print("Result:", result)
t = (1982, 11, 12, 2, 54, 8, 8, 360, 0)
result = time.asctime(t)
print("Result:", result)

# 27. Write a Python program to parse a string representing time and returns the structure time. 
time_string = "22 January, 2020"
print("String representing time:", time_string)
result = time.strptime(time_string, "%d %B, %Y")
print(result)
time_string = "30 Nov 00"
print("\nString representing time:", time_string)
result = time.strptime(time_string, "%d %b %y")
print(result)
time_string = '04/11/15 11:55:23'
print("\nString representing time:", time_string)
result = time.strptime(time_string, "%m/%d/%y %H:%M:%S")
print(result)
time_string = '12-11-2019'
print("\nString representing time:", time_string)
result = time.strptime(time_string, "%m-%d-%Y")
print(result)
time_string = '13::55::26'
print("\nString representing time:", time_string)
result = time.strptime(time_string, "%H::%M::%S")
print(result)




PandasDataframe.py """
DATA SERIES
"""

"""
1. Write a Python program to create and display a one-dimensional array-like
object containing an array of data using Pandas module.
"""
import pandas as pd

x = pd.Series([1,2,3])
print(x)

"""
2. Write a Python program to convert a Panda module Series to Python list and
it's type.
"""
import pandas as pd

x = pd.Series([1,2,3])
print(x)
print(x.tolist())

"""
3. Write a Python program to add, subtract, multiple and divide two Pandas
Series. 
Sample Series: [2, 4, 6, 8, 10], [1, 3, 5, 7, 9]
"""
import pandas as pd

x = pd.Series([2, 4, 6, 8, 10])
y = pd.Series([1, 3, 5, 7, 9])

print(x+y)
print(x-y)
print(x*y)
print(x/y)

"""
4. Write a Python program to compare the elements of the two Pandas Series. 
Sample Series: [2, 3, 3, 8, 10], [1, 3, 5, 7, 9]
"""
import pandas as pd

x = pd.Series([2, 3, 3, 8, 10])
y = pd.Series([1, 3, 5, 7, 9])

print('where x greater than y:\n', x[x > y])
print('where x smaler than y:\n', x[x < y])
print('where x equals y:\n', x[x == y])

"""
DATA FRAMES
"""

"""
Write a Python program to display the following data column wise.
Sample data: {'X':[78,85,96,80,86], 'Y':[84,94,89,83,86],'Z':[86,97,96,72,83]}
"""
import pandas as pd

d = {'X':[78,85,96,80,86], 'Y':[84,94,89,83,86],'Z':[86,97,96,72,83]}
df = pd.DataFrame(d)
print(df)

"""
2. Write a Python program to create and display a DataFrame from a specified
dictionary data which has the index labels.
Sample Python dictionary data and list labels:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np

exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df)

"""
3. Write a Python program to display a summary of the basic information about
a specified DataFrame and its data.
Sample Python dictionary data and list labels:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np

exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df)
print(df.info())
print(df.describe(include='all'))

"""
4. Write a Python program to get the first 3 rows of a given DataFrame.
Sample Python dictionary data and list labels:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df.iloc[:3])
print(df.loc[:'c'])

"""
5. Write a Python program to select the 'name' and 'score' columns from the
following DataFrame.
Sample Python dictionary data and list labels:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[['name','score']])

"""
6. Write a Python program to select the specified columns and rows from a given
data frame.
Select 'name' and 'score' columns in rows 1, 3, 5, 6 from the following data frame.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[['name','score']].iloc[[1,3,5,6]])

"""
7. Write a Python program to select the rows where the number of attempts in the
examination is greater than 2. 
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[df['attempts']>2])

"""
8. Write a Python program to count the number of rows and columns of a DataFrame.
Sample data:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print('number of rows:',len(df.iloc[:]))
print('number of columns:',df.shape[1])

"""
9. Write a Python program to select the rows where the score is missing, i.e. is NaN.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[df.score.isnull()])

"""
10. Write a Python program to select the rows the score is between 15 and 20
(inclusive). 
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[(df.score >=15) & (df.score <=20)])

"""
11. Write a Python program to select the rows where number of attempts in the
examination is less than 2 and score greater than 15.
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df[(df.attempts < 2) & (df.score > 15)])

"""
12. Write a Python program to change the score in row 'd' to 11.5.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
df.score.loc['d'] = 11.5
print(df)

"""
13. Write a Python program to calculate the sum of the examination attempts by
the students. 
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df.attempts.sum())

"""
14. Write a Python program to calculate the mean score for each different
student in DataFrame. 
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
df['mean_score'] = df.score.mean()
print(df)

"""
15. Write a Python program to append a new row 'k' to data frame with given
values for each column. Now delete the new row and return the original DataFrame.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
Values for each column will be:
name : "Suresh", score: 15.5, attempts: 1, qualify: "yes", label: "k"
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)

# solution with append
new_student = pd.Series({'name' : "Suresh", 'score': 15.5, 'attempts': 1, 'qualify': "yes"}, name="k")
df1 = df.append(new_student)
print(df1)
print("original dataframe is unchanged:")
print(df)

# alternative solution:
df.loc['k'] = {'name' : "Suresh", 'score': 15.5, 'attempts': 1, 'qualify': "yes"}
print(df)
print("original is:")
df.drop('k', inplace=True)
print(df)

"""
16. Write a Python program to sort the DataFrame first by 'name' in descending
order, then by 'score' in ascending order. 
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
df.sort_values('name', ascending=False, inplace=True)
print(df)
df.sort_values('score', inplace=True)
print(df)

"""
17. Write a Python program to replace the 'qualify' column contains the values
'yes' and 'no' with True and False.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
df.qualify = df.qualify.map({'yes':True, 'no':False})
print(df)

"""
18. Write a Python program to change the name 'James' to 'Suresh' in name column
of the DataFrame.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df)
df.replace({'name': {'James': 'Suresh'}}, inplace=True)
print(df)

"""
19. Write a Python program to delete the 'attempts' column from the DataFrame.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df= pd.DataFrame(exam_data, labels)
df.drop(columns='attempts', inplace=True)
print(df)

"""
20. Write a Python program to insert a new column in existing DataFrame.
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
df['new_column'] = None
print(df)

"""
21. Write a Python program to iterate over rows in a DataFrame.
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
for row in df.itertuples(index=False, name='Student'):
    print(row)


"""
22. Write a Python program to get list from DataFrame column headers.
Sample data:
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'],
'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
"""
import pandas as pd
import numpy as np
exam_data = {'name': ['Anastasia',
                      'Dima',
                      'Katherine',
                      'James',
                      'Emily',
                      'Michael',
                      'Matthew',
                      'Laura',
                      'Kevin',
                      'Jonas'],
             'score': [12.5, 9, 16.5, np.nan, 9, 20, 14.5, np.nan, 8, 19],
             'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1],
             'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes']}
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
df = pd.DataFrame(exam_data, labels)
print(df.columns.values)




Recursion.py """
1. Write a Python program to calculate the sum of a list of numbers.
"""

def sum_of_list(my_list):
    if not my_list:
        return 0
    elif len(my_list) == 1:
        return my_list[0]
    else:
        return my_list[0] + sum_of_list(my_list[1:])

print(sum_of_list([1,2,3]))

"""
3. Write a Python program of recursion list sum. 
Test Data: [1, 2, [3,4], [5,6]]
Expected Result: 21
"""

def sum_of_list(my_list):
    if not my_list:
        return 0
    elif isinstance(my_list[0], list):
        return sum_of_list(my_list[0]) + sum_of_list(my_list[1:])
    elif len(my_list) == 1:
        return my_list[0]
    else:
        return my_list[0] + sum_of_list(my_list[1:])
        
print(sum_of_list([1, 2, [3,4], [5,6]]))

"""
4. Write a Python program to get the factorial of a non-negative integer.
"""

def factorial(number):
    return 1 if number in [0, 1] else number * factorial(number-1)

print(factorial(2))

"""
5. Write a Python program to solve the Fibonacci sequence using recursion.
"""

#  solution 1: bad function that will work for a low n but will slow the
#  computation drastically as the n increases
def fibonacci(n):
    if n in [1, 2]:
        b = 1
    if n > 2:
        b = fibonacci(n-1) + fibonacci(n-2)
    return b

for i in range(1, 21):
    print(fibonacci(i), end = ' ')
print('\n')

# solution 2: with memoization
# explanation to this solution on Socratica youtube channel at https://youtu.be/Qk0zUZW-U_M
fibonacci_cache = {}
def fibonacci(n):
    if n in fibonacci_cache:
        return fibonacci_cache[n]
    b = 1 if n in [1, 2] else fibonacci(n-1) + fibonacci(n-2)
    fibonacci_cache[n] = b
    return b

for i in range(1, 21):
    print(fibonacci(i), end = ' ')
print('\n')

# solution 3: with memoize function
# more on this at https://www.python-course.eu/python3_memoization.php
def memoize(func):
    memo_cache = {}
    def helper(n):
        if n not in memo_cache:
            memo_cache[n] = func(n)
        return memo_cache[n]
    return helper

@memoize
def fibonacci(n):
    return 1 if n in [1, 2] else fibonacci(n-1) + fibonacci(n-2)

for i in range(1, 21):
    print(fibonacci(i), end = ' ')

"""
6. Write a Python program to get the sum of all digits of a non-negative integer. 
Test Data: 
sumDigits(345) -> 12
sumDigits(45) -> 9
"""

def sum_digits(n):
    return n if n//10 < 1 else n%10 + sum_digits(n//10)

print(sum_digits(345))

"""
7. Write a Python program to calculate the sum of the positive integers
of n+(n-2)+(n-4)... (until n-x =< 0). Go to the editor
Test Data: 
sum_series(6) -> 12
sum_series(10) -> 30
"""

def sum_series(n):
    return n if n <= 2 else n + sum_series(n-2)

print(sum_series(6))

"""
8. Write a Python program to calculate the harmonic sum of n-1. 
Note: The harmonic sum is the sum of reciprocals of the positive integers. 
"""

def harmonic_sum(n):
    return 1 if n == 1 else 1/n + harmonic_sum(n-1)

print(harmonic_sum(5))

"""
9. Write a Python program to calculate the geometric sum of n-1.
Note: In mathematics, a geometric series is a series with a constant
ratio between successive terms. 
"""

def geometric_sum(n, a, r):
    # r is a common ratio, a is a start term
    return a if n == 0 else a*r**n + geometric_sum(n-1, a, r)
print(geometric_sum(5, 1, 2))

"""
10. Write a Python program to calculate the value of 'a' to the power 'b'. 
Test Data : 
(power(3,4) -> 81
"""

def power(a,b):
    return 1 if b == 0 else a*power(a, b-1)

print(power(3,4))

"""
11. Write a Python program to find the greatest common divisor (gcd) of two
integers.
"""

# solution 1 (without recursion)
def find_gcd(num1, num2):
    for i in range(min(num1,num2), 0, -1):
        if num1%i==0 and num2%i==0:
            return i
print(find_gcd(12, 500))

# solution 2
# from w3resource at https://www.w3resource.com/python-exercises/data-structures-and-algorithms/python-recursion-exercise-11.php
def gcd(num1, num2):
    low = min(num1, num2)
    high = max(num1, num2)

    if low == 0:
        return high
    elif low == 1:
        return 1
    else:
        return gcd(low, high%low)
print(gcd(12,500))




RegularExpressions.py """
1. Write a Python program to check that a string contains only a certain set of
characters (in this case a-z, A-Z and 0-9).
"""

import re
my_string = input('enter a string: ')
p = re.compile('[^0-9A-Za-z ]+')
m = p.search(my_string)
if m:
    print('the sting contains', m.group(), 'at place', m.start())
else:
    print('the string contains only digits 0-9 and letters of english alphabet')

"""
2. Write a Python program that matches a string that has an 'a' followed by zero
or more b's.
"""

import re
my_string = input('enter a string ')
p = re.compile('ab*?')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
3. Write a Python program that matches a string that has an 'a' followed by
one or more b's.
"""

import re
my_string = input('enter a string ')
p = re.compile('ab+?')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
4. Write a Python program that matches a string that has an 'a' followed by
zero or one 'b'.
"""

import re
my_string = input('enter a string ')
p = re.compile('ab??')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
5. Write a Python program that matches a string that has an 'a' followed by
three 'b'.
"""

import re
my_string = input('enter a string ')
p = re.compile('ab{3}?')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
6. Write a Python program that matches a string that has an 'a' followed by
two to three 'b'.
"""

import re
my_string = input('enter a string ')
p = re.compile('ab{2,3}?')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
7. Write a Python program to find sequences of lowercase letters joined with
an underscore.
"""

import re
my_string = input('enter a string ')
p = re.compile('[a-z]+_[a-z]+')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
8. Write a Python program to find sequences of one upper case letter followed
by lower case letters.
"""

import re
my_string = input('enter a string ')
p = re.compile('[A-Z]+[a-z]+')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
9. Write a Python program that matches a string that has an 'a' followed by
anything, ending in 'b'.
"""

import re
my_string = input('enter a string ')
p = re.compile('a.*?b$')
m = p.search(my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
10. Write a Python program that matches a word at the beginning of a string.
"""

import re
my_string = input('enter a string ')
m = re.match('^\w+', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
11. Write a Python program that matches a word at end of string, with optional
punctuation.
"""

import re
my_string = input('enter a string ')
m = re.search('\b\w+[.!?,;]*$', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
12. Write a Python program that matches a word containing 'z'.
"""

import re
my_string = input('enter a string ')
m = re.search('\w*z\w*', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
13. Write a Python program that matches a word containing 'z', not start or
end of the word.
"""

import re
my_string = input('enter a string ')
m = re.search('\w+z\w+', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
14. Write a Python program to match a string that contains only upper and
lowercase letters, numbers, and underscores.
"""

import re
my_string = input('enter a string ')
m = re.search('[^0-9A-Za-z_]+', my_string)
if m:
    print('no match found')
else:
    print('it\'s a match')

"""
15. Write a Python program where a string will start with a specific number.
"""

import re
my_string = input('enter a string ')
my_number = input('enter a number ')
m = re.match(my_number, my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
16. Write a Python program to remove leading zeros from an IP address.
"""

import re
my_IP = input('enter a string ')
my_clean_IP = re.sub('\.0*', '.', my_IP)
print(my_clean_IP)

"""
17. Write a Python program to check for a number at the end of a string.
"""

import re
my_string = input('enter a string ')
my_number = input('enter a number ')
m = re.search(f'{my_number}$', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
18. Write a Python program to search the numbers (0-9) of length between
1 to 3 in a given string.
"""

import re
my_string = input('enter a string ')
m = re.search('[0-9]{1,3}', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
19. Write a Python program to search some literals strings in a string. 
Sample text : 'The quick brown fox jumps over the lazy dog.'
Searched words : 'fox', 'dog', 'horse'
"""

import re
my_string = 'The quick brown fox jumps over the lazy dog.'
m = re.search('cat|dog|fox|horse', my_string)
if m:
    print('it\'s a match')
else:
    print('no match found')

"""
20. Write a Python program to search a literals string in a string and also
find the location within the original string where the pattern occurs. 
Sample text : 'The quick brown fox jumps over the lazy dog.'
Searched words : 'fox'
"""

import re
my_string = 'The quick brown fox jumps over the lazy dog.'
m = re.search('\Wfox\W', my_string)
if m:
    print('it\'s a match, starts on', m.start())
else:
    print('no match found')

"""
21. Write a Python program to find the substrings within a string. 
Sample text : 'Python exercises, PHP exercises, C# exercises'
Pattern : 'exercises'
Note: There are two instances of exercises in the input string.
"""

import re
my_string = 'Python exercises, PHP exercises, C# exercises'
my_substring = 'exercises'
m = re.findall(my_substring, my_string)
if m:
    print('it\'s a match', len(m))
else:
    print('no match found')

"""
22. Write a Python program to find the occurrence and position of the
substrings within a string.
"""

import re
my_string = 'Python exercises, PHP exercises, C# exercises'
my_substring = 'exercises'
m = re.finditer(my_substring, my_string)
for match in m:
    print(f"string \'{my_substring}\'", 'found at position', match.span())

"""
23. Write a Python program to replace whitespaces with an underscore and
vice versa.
"""

import re
my_string = input('enter a sentence: ')
m = re.sub('\s', '_', my_string)
print(m)
m1 = re.sub('_', ' ', m)
print(m1)

"""
25. Write a Python program to convert a date of yyyy-mm-dd format to dd-mm-yyyy
format.
"""

import re
date = '2018-03-31'
m = re.split('-', date)
new_date = '-'.join(m[::-1])
print(new_date)

"""
26. Write a Python program to match if two words from a list of words starting
with letter 'P'.
"""

import re
my_list = ['Python Program', 'hello world', 'python program']
for phrase in my_list:
    if re.match('P\w*\sP\w*', phrase):
        print(phrase)

"""
27. Write a Python program to separate and print the numbers of a given string.
"""




Requests.py # Requests is an elegant and simple HTTP library for Python, built for human beings. Requests allows you to send HTTP/1.1 requests extremely easily. There's no need to manually add query strings to your URLs, or to form-encode your POST data. Keep-alive and HTTP connection pooling are 100 % automatic.

# 1. Write a Python code to find the Requests module - version, licence, copyright information, author, author email, document url, title and description. 
import requests
print("Python Requests module - version:", requests.__version__)
print("Licence:", requests. __license__)
print("Copyright:", requests.__copyright__)
print("Author:", requests.__author__)
print("Author email:", requests.__author_email__)
print("Document url, title, description:")
print(requests.__url__)
print(requests.__title__)
print(requests.__description__)

# 2. Write a Python code to check the status code issued by a server in response to a client's request made to the server. Print all of the methods and attributes available to objects on successful request. 
res = requests.get('https://google.com/')
print("Response of https://google.com/:")
print(res.status_code)
res = requests.get('https://amazon.com/')
print("Response of https://amazon.com/:")
print(res.status_code)
res = requests.get('https://w3resource.com/')
print("Response of https://w3resource.com/:")
print(res.status_code)
print("\nMethods and attributes available to objects on successful\nrequest of https://w3resource.com:\n")
print(dir(res))

# 3. Write a Python code to send a request to a web page, and print the response text and content. Also get the raw socket response from the server. 
res = requests.get('https://www.google.com/')
print("Response text of https://google.com/:")
print(res.text)
print("\n==============================================================================")
print("\nContent of the said url:")
print(res.content)
print("\n==============================================================================")
print("\nRaw data of the said url:")
r = requests.get('https://api.github.com/events', stream=True)
print(r.raw)
print(r.raw.read(15))

# 4. Write a Python code to send a request to a web page, and print the information of headers. Also parse these values and print key-value pairs holding various information. 
r = requests.get('https://api.github.com/')
response = r.headers
print("Headers information of the said response:")
print(response)
print("\nVarious Key-value pairs information of the said resource and request:")

print("Date: ", r.headers['date'])
print("server: ", r.headers['server'])
print("status: ", r.headers['status'])
print("cache-control: ", r.headers['cache-control'])
print("vary: ", r.headers['vary'])
print("x-github-media-type: ", r.headers['x-github-media-type'])
print("access-control-expose-headers: ",
      r.headers['access-control-expose-headers'])
print("strict-transport-security: ", r.headers['strict-transport-security'])
print("x-content-type-options: ", r.headers['x-content-type-options'])
print("x-xss-protection: ", r.headers['x-xss-protection'])
print("referrer-policy: ", r.headers['referrer-policy'])
print("content-security-policy: ", r.headers['content-security-policy'])
print("content-encoding: ", r.headers['content-encoding'])
print("X-Ratelimit-Remaining: ", r.headers['X-Ratelimit-Remaining'])
print("X-Ratelimit-Reset: ", r.headers['X-Ratelimit-Reset'])
print("X-Ratelimit-Used: ", r.headers['X-Ratelimit-Used'])
print("Accept-Ranges:", r.headers['Accept-Ranges'])
print("X-GitHub-Request-Id:", r.headers['X-GitHub-Request-Id'])

# 5. Write a Python code to send a request to a web page, and print the JSON value of the response. Also print each key value of the response. 
r = requests.get('https://api.github.com/')
response = r.json()
print("JSON value of the said response:")
print(r.json())
print("\nEach key of the response:")
print("Current user url:", response['current_user_url'])
print("Current user authorizations html url:",
      response['current_user_authorizations_html_url'])
print("Authorizations url:", response['authorizations_url'])
print("code_search_url:", response['code_search_url'])
print("commit_search_url:", response['commit_search_url'])
print("Emails url:", response['emails_url'])
print("Emojis url:", response['emojis_url'])
print("Events url:", response['events_url'])
print("Feeds url:", response['feeds_url'])
print("Followers url:", response['followers_url'])
print("Following url:", response['following_url'])
print("Gists url:", response['gists_url'])
print("Issue search url:", response['issue_search_url'])
print("Issues url:", response['issues_url'])
print("Keys url:", response['keys_url'])
print("label search url:", response['label_search_url'])
print("Notifications url:", response['notifications_url'])
print("Organization url:", response['organization_url'])
print("Organization repositories url:",
      response['organization_repositories_url'])
print("Organization teams url:", response['organization_teams_url'])
print("Public gists url:", response['public_gists_url'])
print("Rate limit url:", response['rate_limit_url'])
print("Repository url:", response['repository_url'])
print("Repository search url:", response['repository_search_url'])
print("Current user repositories url:",
      response['current_user_repositories_url'])
print("Starred url:", response['starred_url'])
print("Starred gists url:", response['starred_gists_url'])
print("User url:", response['user_url'])
print("User organizations url:", response['user_organizations_url'])
print("User repositories url:", response['user_repositories_url'])
print("User search url:", response['user_search_url'])

# 6. Write a Python code to send a request to a web page and stop waiting for a response after a given number of seconds. In the event of times out of request, raise Timeout exception. 
print("timeout = 0.001")
try:
    r = requests.get('https://github.com/', timeout=0.001)
    print(r.text)
except requests.exceptions.RequestException as e:
    print(e)

print("\ntimeout = 1.0")
try:
    r = requests.get('https://github.com/', timeout=1.0)
    print("Connected....!")
except requests.exceptions.RequestException as e:
    print(e)

# 7. Write a Python code to send some sort of data in the URL's query string. 
payload = {'key1': 'value1', 'key2': 'value2'}
print("Parameters: ", payload)
r = requests.get('https://httpbin.org/get', params=payload)
print("Print the url to check the URL has been correctly encoded or not!")
print(r.url)
print("\nPass a list of items as a value:")
payload = {'key1': 'value1', 'key2': ['value2', 'value3']}
print("Parameters: ", payload)
r = requests.get('https://httpbin.org/get', params=payload)
print("Print the url to check the URL has been correctly encoded or not!")
print(r.url)

# 8. Write a Python code to send cookies to a given server and access cookies from the response of a server. 
url = 'http://httpbin.org/cookies'
# A dictionary (my_cookies) of cookies to send to the specified url.
my_cookies = dict(
    cookies_are='Cookies parameter use to send cookies to the server')
r = requests.get(url, cookies=my_cookies)
print(r.text)
# Accessing cookies with Requests
# url = 'http://WebsiteName/cookie/setting/url'
# res = requests.get(url)
# Value of cookies
# print(res.cookies['cookie_name'])

# 9. Write a Python code to verify the SSL certificate for a website which is certified. 
#Requests ignore verifying the SSL certificate if you set verify to False
# Making a get request
response = requests.get('https://rigaux.org/', verify=False)
print(response)
print("\n=======================================================\n")
#Requests verifies SSL certificates for HTTPS requests, just like a web browser.
response1 = requests.get('https://google.com/')
print(response1)
print("\n=======================================================\n")
#Requests ignore verifying the SSL certificate if you set verify to True (Default value)
response1 = requests.get('https://rigaux.org/', verify=True)
print(response1)




Searching&Sorting.py """
SEARCH
"""

"""
1. Write a Python program for binary search. 
"""

# solution 1 (changing location of 'mid' item untill 'first' and 'last' are the same item)
def binary_search(my_list, x):
    first = 0
    last = len(my_list)-1
    while first <= last:
        mid = (first+last)//2
        if x == my_list[mid]:
            return True
        elif x > my_list[mid]:
            first = mid + 1
        else:
            last = mid - 1
    return False

print(binary_search([1,2,3,5,8], 6))
print(binary_search([1,2,3,5,8], 1))

# solution 2 (similar to 1, slicing the list untill list length is 0)
def binary_search(my_list, x):
    while len(my_list) > 0:
        last = len(my_list)-1
        mid = last//2
        if x == my_list[mid]:
            return True
        elif x > my_list[mid]:
            my_list = my_list[mid+1:]
        else:
            my_list = my_list[:mid]
    return False

print(binary_search([1,2,3,5,8], 6))
print(binary_search([1,2,3,5,8], 1))

# solution 3 (with recursion)
def binary_search(my_list, x):
    if len(my_list) == 0:
        return False
    mid = len(my_list)//2
    if x == my_list[mid]:
        return True
    elif x > my_list[mid]:
        return binary_search(my_list[mid+1:], x)
    else:
        return binary_search(my_list[:mid], x)

print(binary_search([1,2,3,5,8], 6))
print(binary_search([1,2,3,5,8], 1))

"""
2. Write a Python program for sequential search. 
"""

def sequential_search(my_list, x):
    return any(item == x for item in my_list)

print(sequential_search([11,23,58,31,56,77,43,12,65,19], 11))

"""
SORT
"""

"""
4. Write a Python program to sort a list of elements using the bubble sort algorithm.
"""

# solution 1
def buble_sort(my_list):
    # counting number of "no_swap" pairs (naigbors that are in the right order),
    # when list is sorted the number of "no_swaps" will equal to len(list)-1
    # for each "walk" through the list, the "no_swaps" number is reset back to 0
    
    no_swaps = 0
    while no_swaps < (len(my_list)-1):
        no_swaps = 0
        for i in range(len(my_list)-1):
            if my_list[i] > my_list[i+1]:
                temp = my_list[i]
                my_list[i] = my_list[i+1]
                my_list[i+1] = temp
            else:
                no_swaps += 1

x = [14,46,43,27,57,41,45,21,70,15]
buble_sort(x)
print(x)
y = [5,1,4,2,8]
buble_sort(y)
print(y)

"""
5. Write a Python program to sort a list of elements using the selection sort algorithm.
"""

def selection_sort(my_list):
    for i in range(len(my_list)-1, 0, -1):
        maximum = max(my_list[:i])
        max_ind = my_list.index(maximum)
        if maximum > my_list[i]:
            temp = maximum
            my_list[max_ind] = my_list[i]
            my_list[i] = temp
            
x = [14,46,43,27,57,41,45,21,70,15]
selection_sort(x)
print(x)
y = [5,1,4,2,8]
selection_sort(y)
print(y)

"""
6. Write a Python program to sort a list of elements using the insertion sort algorithm.
"""

def insertion_sort(my_list):
    # for each element in the list (first for loop), we compare it to each of the
    # preciding elements (second for loop), and move it to it's sorted position
    # relative to it's preciding elements; this way for each next element 
    # the elements before it are already in the correct sorted order
    for i in range(1,len(my_list)):
        loc = i
        for j in range(loc - 1, -1, -1):
            if  my_list[loc] < my_list[j]:
                temp = my_list[loc]
                my_list[loc] = my_list[j]
                my_list[j] = temp
                loc = j
              
x = [14,46,43,27,57,41,45,21,70,15]
insertion_sort(x)
print(x)
y = [5,1,4,2,8]
insertion_sort(y)
print(y)

"""
7. Write a Python program to sort a list of elements using shell sort algorithm.
"""

def shell_sort(my_list):
    # each gap is the "sublist", i.e. gap=1 is the whole list, gap = 2 is half the list elements etc.
    gap = len(my_list)//2
    while gap > 0:
        print("gap=",gap)
        print("before this gap sort", my_list)
        # for each element in the list (first for loop), compare it to every
        # preciding elements of it's "sublist" (second for loop).
        # "sublist" is defined by "gap"
        for i in range(gap,len(my_list)):
            print('i=',i)
            loc = i
            for j in range(loc - gap, -1, -gap):
                if  my_list[loc] < my_list[j]:
                    temp = my_list[loc]
                    my_list[loc] = my_list[j]
                    my_list[j] = temp
                    loc = j
                    print(my_list)
        gap = gap//2
        print(">",my_list)
        
x = [89,14,46,43,27,57,41,45,21,70,15,1,90]
shell_sort(x)
print(x)
y = [5,1,4,2,8]
shell_sort(y)
print(y)

"""
8. Write a Python program to sort a list of elements using the merge sort algorithm.
"""
# solution from http://interactivepython.org/runestone/static/pythonds/SortSearch/TheMergeSort.html

def merge_sort(alist):
    if len(alist) <= 1:
        return
    mid = len(alist)//2
    lefthalf = alist[:mid]
    righthalf = alist[mid:]

    merge_sort(lefthalf)
    merge_sort(righthalf)

    i=0
    j=0
    k=0

    while i < len(lefthalf) and j < len(righthalf):
        if lefthalf[i] < righthalf[j]:
            alist[k]=lefthalf[i]
            i += 1
        else:
            alist[k]=righthalf[j]
            j += 1
        k=k+1

        # elements on leftside that were left "unmerged"
        #(because they are bigger than the rest)
    while i < len(lefthalf):
        alist[k]=lefthalf[i]
        i += 1
        k=k+1

        # elemnts on rightside that were left "unmerged"
        #(because they are bigger than the rest)
    while j < len(righthalf):
        alist[k]=righthalf[j]
        j += 1
        k=k+1
        
x = [14,46,43,27,57,41,45,21,70,15,1]
merge_sort(x)
print(x)
y = [5,1,4,2,8]
merge_sort(y)
print(y)

"""
9. Write a Python program to sort a list of elements using the quick sort algorithm.
"""

def quick_sort(my_list):
    
    def quick_sort_recur(my_list, start, end):
        # inner helper function to help perform the quick sort recursively each
        # time updating the start and end of the 'working section' of the list

        # check if the 'working section' of the list (from 'start' to 'end' index)
        # has more than 1 element, otherwise it is already 'sorted'
        if start < end:
            
            pivot_ind = start           # pivot value index
            left_ind = pivot_ind + 1    # leftside, start index 
            right_ind = end             # rightside, end index

            # check untill the left and right indexes cross, i.e. untill all the
            # elements of the section are checked and moved if needed
            while left_ind <= right_ind:
                
                # if leftside value is lower than pivot value,
                # move left index one place to the right
                if my_list[left_ind] < my_list[pivot_ind]:
                    left_ind += 1
                    
                # if rightside value is higher than pivot value,
                # move right index one place to the left
                elif my_list[right_ind] > my_list[pivot_ind]:
                    right_ind -= 1
                    
                # if leftside value is higher and rightside is lower than
                # pivot value, swap the left and right values, and move both
                # indexes inward
                else:
                    temp = my_list[left_ind]
                    my_list[left_ind] = my_list[right_ind]
                    my_list[right_ind] = temp
                    left_ind += 1
                    right_ind -= 1

            # when right and left indexes have crossed (section is finished),
            # move the pivot value to separate left(lower values side) and
            # right(higher values side)
            temp = my_list[pivot_ind]
            my_list[pivot_ind] = my_list[right_ind]
            my_list[right_ind] = temp

            # repeat for leftside of the pivot value, the end of the new section
            # is where the pivot value was moved not including (right index - 1), 
            # there is no need to include it as it is already in its right
            # position in the sorted list
            quick_sort_recur(my_list, pivot_ind, right_ind-1)
            
            # repeat for rightside of the pivot value, the beginning of the new section
            # is where the pivot value was moved not inlcuding (right index + 1)
            quick_sort_recur(my_list, right_ind+1, end)

    # first call to the quick_sort_recut function with the start=0 and end=last index of the list         
    quick_sort_recur(my_list, 0, len(my_list)-1)            
        
x = [48,14,46,43,21,57,41,45,27,70,15,1]
quick_sort(x)
print(x)
y = [5,1,4,2,8]
quick_sort(y)
print(y)
            
"""
10. Write a Python program for counting sort.
"""
# helped me to solve very easily: https://brilliant.org/wiki/counting-sort/

def counting_sort(my_list):
    # 'counter' is the list that for each integer element of 'my_list' will store
    # the number of times this integer appears in 'my_list' + number of inetgers that
    # are lower than this integer, so each integer must have index == this integer in 'counter'
    counter = [0] * (max(my_list)+1)

    # 'out' is the final sorted 'my_list' the function will return, it must have same length
    out = [0] * len(my_list)

    # the number of times each integer appears in 'my_list' is stored in counter
    # under index that equals to this integer
    for i in my_list:
        counter[i] += 1

    # each element in counter is incremented by the sum of all the preceding elements
    for i in range(1, len(counter)):
        counter[i] = counter[i] + counter[i-1]

    # each inetger starting from the end of my_list is added to the 'out' list
    # at the index position determined by the value that is stored in 'counter' list
    # under the index == this integer element
    # the value in 'counter' is reduced by 1, as one slot is taken
    for i in my_list[::-1]:
        ind = counter[i]-1
        out[ind] = i
        counter[i] -= 1
    
    return out

x = [48,14,46,43,21,57,41,45,27,70,15,1]
print(counting_sort(x))
y = [5,1,4,2,8]
print(counting_sort(y))




Set.py """
1. Write a Python program to create a set.
"""


my_set = set()
my_second_set = {'a', 'b'}
print(my_set, type(my_set))
print(my_second_set, type(my_second_set))

"""
2. Write a Python program to iterate over sets.
"""

my_set = {'a', 'b'}

for i in my_set:
    print(i)

"""
3. Write a Python program to add member(s) in a set.
"""

my_set = {'a', 'b', 'c'}
print(my_set)

"""
4. Write a Python program to remove item(s) from set
"""

my_set = {'a', 'b'}
my_set.discard('a')
print(my_set)

my_set = {'a', 'b'}
my_set.remove('a')
print(my_set)

my_set = {'a', 'b'}
my_set.pop()
print(my_set)

"""
5. Write a Python program to remove an item from a set if it is present in the set.
"""

my_set = {'a', 'b'}
my_set.discard('a')
my_set.discard('c')
print(my_set)

"""
6. Write a Python program to create an intersection of sets.
"""
set_one = {'a', 'b', 'c'}
set_two = {'a', 'x', 'f'}

set_inter1 = set_one.intersection(set_two)
set_inter2 = set_one & set_two

print(set_inter1)
print(set_inter2)

"""
7. Write a Python program to create a union of sets.
"""

set_one = {'a', 'b', 'c'}
set_two = {'a', 'x', 'f'}

set_inter1 = set_one.union(set_two)
set_inter2 = set_one | set_two

print(set_inter1)
print(set_inter2)

"""
8. Write a Python program to create set difference.
"""

set_one = {'a', 'b', 'c'}
set_two = {'a', 'x', 'f'}

set_inter1 = set_one.difference(set_two)
set_inter2 = set_one - set_two

print(set_inter1)
print(set_inter2)

"""
9. Write a Python program to create a symmetric difference.
"""

set_one = {'a', 'b', 'c'}
set_two = {'a', 'x', 'f'}

set_inter1 = set_one.symmetric_difference(set_two)
set_inter2 = set_one ^ set_two

print(set_inter1)
print(set_inter2)

"""
10. Write a Python program to issubset and issuperset.
"""

set_one = {'a', 'b', 'c'}
set_two = {'a', 'b'}

print(set_two.issubset(set_one))
print(set_one.issuperset(set_two))

"""
11. Write a Python program to create a shallow copy of sets. 
Note : Shallow copy is a bit-wise copy of an object. A new object is created
that has an exact copy of the values in the original object.
"""

set_one = {'a', 'b', 'c'}
set_two = set_one.copy()
print(set_one)
print(set_two)

"""
12. Write a Python program to clear a set.
"""

set_one = {'a', 'b', 'c'}
set_one.clear()
print(set_one)

"""
13. Write a Python program to use of frozensets.
"""

set_one = frozenset((1, 2, 3))
set_two = {1, 2, 3}
list_one = [4, 5, 6]
print (set_one == set_two)
print(set_one.isdisjoint(list_one))

"""
14. Write a Python program to find maximum and the minimum value in a set.
"""

set_one = frozenset((1, 2, 3))
print(max(set_one))
print(min(set_one))

"""
15. Write a Python program to find the length of a set.
"""

set_one = frozenset((1, 2, 3))
print(len(set_one))
length = len(set_one)
print(length)




SQLLiteDB.py # 1. Write a Python program to create a SQLite database and connect with the database and print the version of the SQLite database.
import io
from sqlite3 import Error
import sqlite3
try:
    conn = sqlite3.connect('temp.db')
    c = conn.cursor()
    print("\nDatabase created and connected to SQLite.")
    c.execute("select sqlite_version();")
    record = c.fetchall()
    print("\nSQLite Database Version is: ", record)
    c.close()
except sqlite3.Error as error:
    print("\nError while connecting to sqlite", error)
finally:
    if (conn):
        conn.close()
        print("\nThe SQLite connection is closed.")

# 2. Write a Python program to create a SQLite database connection to a database that resides in the memory.
import sqlite3
try:
    conn = sqlite3.connect('temp.db')
    c = sqlite3.connect(':memory:')
    print("\nMemory database created and connected to SQLite.")
    c.execute("select sqlite_version();")
    print("\nSQLite Database Version is: ", sqlite3.version)
    c.close()
except sqlite3.Error as error:
    print("\nError while connecting to sqlite", error)
finally:
    if (conn):
        conn.close()
        print("\nThe SQLite connection is closed.")

# 3. Write a Python program to connect a database and create a SQLite table within the database.
import sqlite3
# connect to the database.
c = sqlite3.connect('mydatabase.db')
# defining a cursor
cursor = c.cursor()
# creating the table (schema) agent_master
cursor.execute("""
CREATE TABLE agent_master(agent_code char(6),
agent_name char(40),working_area char(35),
commission decimal(10,2),phone_no char(15) NULL);
""")
print("\nagent_master file has created.")
# disconnecting ...
c . close()
print("\nThe SQLite connection is closed.")

# 4. Write a Python program to list the tables of given SQLite database file.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create two tables
    cursorObj.execute(
        "CREATE TABLE agent_master(agent_code char(6),agent_name char(40),working_area char(35),commission decimal(10,2),phone_no char(15) NULL);")
    cursorObj.execute(
        "CREATE TABLE temp_agent_master(agent_code char(6),agent_name char(40),working_area char(35),commission decimal(10,2),phone_no char(15) NULL);")
    print("List of tables:")
    cursorObj.execute("SELECT name FROM sqlite_master WHERE type='table';")
    print(cursorObj.fetchall())
    c.commit()


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 5. Write a Python program to create a table and insert some records in that table. Finally selects all rows from the table and display the records.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
# Insert records
    cursorObj.executescript("""
   INSERT INTO salesman VALUES(5001,'James Hoog', 'New York', 0.15);
   INSERT INTO salesman VALUES(5002,'Nail Knite', 'Paris', 0.25);
   INSERT INTO salesman VALUES(5003,'Pit Alex', 'London', 0.15);
   INSERT INTO salesman VALUES(5004,'Mc Lyon', 'Paris', 0.35);
   INSERT INTO salesman VALUES(5005,'Paul Adam', 'Rome', 0.45);
   """)
    c.commit()
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("Agent details:")
    for row in rows:
        print(row)


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 6. Write a Python program to insert a list of records into a given SQLite table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c, rows):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
    sqlite_insert_query = """INSERT INTO salesman
                          (salesman_id, name, city, commission) 
                          VALUES (?, ?, ?, ?);"""
    cursorObj.executemany(sqlite_insert_query, rows)
    c.commit()
    print("Number of records after inserting rows:")
    cursor = cursorObj.execute('select * from salesman;')
    print(len(cursor.fetchall()))


# Insert records
rows = [(5001, 'James Hoog', 'New York', 0.15),
        (5002, 'Nail Knite', 'Paris', 0.25),
        (5003, 'Pit Alex', 'London', 0.15),
        (5004, 'Mc Lyon', 'Paris', 0.35),
        (5005, 'Paul Adam', 'Rome', 0.45)]

conn = sql_connection()
sql_table(conn, rows)

if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 7. Write a Python program to insert values to a table from user input.
c = sqlite3 . connect('mydatabase.db')
cursor = c.cursor()
# create the salesman table
cursor.execute(
    "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")

s_id = input('Salesman ID:')
s_name = input('Name:')
s_city = input('City:')
s_commision = input('Commission:')
cursor.execute("""
INSERT INTO salesman(salesman_id, name, city, commission)
VALUES (?,?,?,?)
""", (s_id, s_name, s_city, s_commision))
c.commit()
print('Data entered successfully.')
c . close()
if (c):
    c.close()
    print("\nThe SQLite connection is closed.")

# 8. Write a Python program to count the number of rows of a given SQLite table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
    print("Number of records before inserting rows:")
    cursor = cursorObj.execute('select * from salesman;')
    print(len(cursor.fetchall()))
# Insert records
    cursorObj.executescript("""
    INSERT INTO salesman VALUES(5001,'James Hoog', 'New York', 0.15);
    INSERT INTO salesman VALUES(5002,'Nail Knite', 'Paris', 0.25);
    INSERT INTO salesman VALUES(5003,'Pit Alex', 'London', 0.15);
    INSERT INTO salesman VALUES(5004,'Mc Lyon', 'Paris', 0.35);
    INSERT INTO salesman VALUES(5005,'Paul Adam', 'Rome', 0.45);
    """)
    c.commit()
    print("\nNumber of records after inserting rows:")
    cursor = cursorObj.execute('select * from salesman;')
    print(len(cursor.fetchall()))


conn = sql_connection()
sql_table(conn)

if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 9. Write a Python program to update a specific column value of a given table and select all rows before and after updating the said table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
# Insert records
    cursorObj.executescript("""
    INSERT INTO salesman VALUES(5001,'James Hoog', 'New York', 0.15);
    INSERT INTO salesman VALUES(5002,'Nail Knite', 'Paris', 0.25);
    INSERT INTO salesman VALUES(5003,'Pit Alex', 'London', 0.15);
    INSERT INTO salesman VALUES(5004,'Mc Lyon', 'Paris', 0.35);
    INSERT INTO salesman VALUES(5005,'Paul Adam', 'Rome', 0.45);
    """)
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("Agent details:")
    for row in rows:
        print(row)
    print("\nUpdate commission .15 to .45 where id is 5003:")
    sql_update_query = """Update salesman set commission = .45 where salesman_id = 5003"""
    cursorObj.execute(sql_update_query)
    c.commit()
    print("Record Updated successfully ")
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("\nAfter updating Agent details:")
    for row in rows:
        print(row)


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 10. Write a Python program to update all the values of a specific column of a given SQLite table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
# Insert records
    cursorObj.executescript("""
    INSERT INTO salesman VALUES(5001,'James Hoog', 'New York', 0.15);
    INSERT INTO salesman VALUES(5002,'Nail Knite', 'Paris', 0.25);
    INSERT INTO salesman VALUES(5003,'Pit Alex', 'London', 0.15);
    INSERT INTO salesman VALUES(5004,'Mc Lyon', 'Paris', 0.35);
    INSERT INTO salesman VALUES(5005,'Paul Adam', 'Rome', 0.45);
    """)
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("Agent details:")
    for row in rows:
        print(row)
    print("\nUpdate all commision to .55:")
    sql_update_query = """Update salesman set commission = .55"""
    cursorObj.execute(sql_update_query)
    c.commit()
    print("Record Updated successfully ")
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("\nAfter updating Agent details:")
    for row in rows:
        print(row)


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 11. Write a Python program to delete a specific row from a given SQLite table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
# Create the table
    cursorObj.execute(
        "CREATE TABLE salesman(salesman_id n(5), name char(30), city char(35), commission decimal(7,2));")
# Insert records
    cursorObj.executescript("""
   INSERT INTO salesman VALUES(5001,'James Hoog', 'New York', 0.15);
   INSERT INTO salesman VALUES(5002,'Nail Knite', 'Paris', 0.25);
   INSERT INTO salesman VALUES(5003,'Pit Alex', 'London', 0.15);
   INSERT INTO salesman VALUES(5004,'Mc Lyon', 'Paris', 0.35);
   INSERT INTO salesman VALUES(5005,'Paul Adam', 'Rome', 0.45);
   """)
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("Agent details:")
    for row in rows:
        print(row)
    print("\nDelete Salesman of ID 5003:")
    s_id = 5003
    cursorObj.execute("""
   DELETE FROM salesman
   WHERE salesman_id = ?
   """, (s_id,))
    c.commit()
    cursorObj.execute("SELECT * FROM salesman")
    rows = cursorObj.fetchall()
    print("\nAfter updating Agent details:")
    for row in rows:
        print(row)


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 12. Write a Python program to alter a given SQLite table.


def sql_connection():
    try:
        return sqlite3.connect('mydatabase.db')
    except Error:
        print(Error)


def sql_table(c):
    cursorObj = c.cursor()
    cursorObj.execute(
        "CREATE TABLE agent_master(agent_code char(6),agent_name char(40),working_area char(35),commission decimal(10,2),phone_no char(15) NULL);")
    print("\nagent_master file has created.")

    # adding a new column in the agent_master table
    cursorObj.execute("""
   ALTER TABLE agent_master
   ADD COLUMN FLAG BOOLEAN;
   """)
    print("\nagent_master file altered.")
    c.commit()


conn = sql_connection()
sql_table(conn)
if (conn):
    conn.close()
    print("\nThe SQLite connection is closed.")

# 13. Write a Python program to create a backup of a SQLite database.
c = sqlite3.connect('mydatabase.db')
with io.open('clientes_dump.sql', 'w') as f:
    for linha in c.iterdump():
        f.write('%s\n' % linha)
print('Backup performed successfully.')
print('Saved as mydatabase_dump.sql')
c.close()




String.py """
1.calculate the length of a string
"""

import string
from math import pi
my_string = input("enter a string ")

# solution 1:
print(len(my_string))

# solution 2:
length = 0
for letter in my_string:
    length += 1

print(length)

"""
2.count the number of characters
(character frequency) in a string. 
Sample String : 'google.com'
Expected Result : {'o': 3, 'g': 2, '.': 1, 'e': 1, 'l': 1, 'm': 1, 'c': 1}
"""

my_string = input("enter a string ")

# solution 1:
my_dict = {}
for letter in my_string:
    my_dict[letter] = my_string.count(letter)
print(my_dict)

# solution 2:
my_dict = {}
for letter in my_string:
    my_dict[letter] = 0

for letter in my_string:
    my_dict[letter] += 1
print(my_dict)

# solution 3:
my_dict = {}
for letter in my_string:
    if letter in my_dict:
        my_dict[letter] += 1
    else:
        my_dict[letter] = 1
print(my_dict)

"""
3.get a string made of the first 2
and the last 2 chars from a given string.
If the string length is less than 2, return 'empty string'.
Sample String : 'w3resource'
Expected Result : 'w3ce'
Sample String : 'w3'
Expected Result : 'w3w3'
Sample String : ' w'
Expected Result : Empty String
"""

my_string = input("enter a string ")

# solution 1
if len(my_string) > 1:
    print(my_string[:2]+my_string[-2:])
else:
    print('empty string')

"""
4.get a string from a given string
where all occurrences of its first char have been changed to '$',
except the first char itself. 
Sample String : 'restart'
Expected Result : 'resta$t'
"""

my_string = input("enter a string ")

# solution 1
new_string = my_string[0]
for i in range(1, len(my_string)):
    if my_string[i] == my_string[0]:
        new_string += '$'
    else:
        new_string += my_string[i]
print(new_string)

# solution 2
new_string = ''
for letter in my_string:
    if letter == my_string[0]:
        new_string += '$'
    else:
        new_string += letter
my_list = list(new_string)
my_list[0] = my_string[0]
new_string = ''.join(my_list)
print(new_string)

# solution 3
first_letter = my_string[0]
helper_string = my_string.replace(first_letter, '$')
new_string = first_letter + helper_string[1:]
print(new_string)

"""
5.get a single string from two given strings,
separated by a space and swap the first two characters of each string. 
Sample String : 'abc', 'xyz' 
Expected Result : 'xyc abz'
"""

string_one = input("enter first string ")
string_two = input("enter second string ")

# solution 1
new_string = string_two[:2]+string_one[2:]+' '+string_one[:2]+string_two[2:]
print(new_string)

"""
6.add 'ing' at the end of a given string
(length should be at least 3). If the given string already ends with 'ing'
then add 'ly' instead. If the string length of the given string is less
than 3, leave it unchanged.  
Sample String : 'abc'
Expected Result : 'abcing' 
Sample String : 'string'
Expected Result : 'stringly'
"""

my_string = input("enter a string ")

# solution 1
if len(my_string) < 3:
    new_string = my_string
elif my_string[-3:] == 'ing':
    new_string = my_string + 'ly'
else:
    new_string = my_string + 'ing'
print(new_string)

"""
7.find the first appearance of the substring
'not' and 'poor' from a given string, if 'poor' follows the 'not', replace
the whole 'not'...'poor' substring with 'good'. Return the resulting string.
Sample String : 'The lyrics is not that poor!'
Expected Result : 'The lyrics is good!'
"""

my_string = input("enter a string ")

# solution 1
not_position = my_string.find('not')
poor_position = my_string.find('poor')

if not_position < poor_position and not_position != -1:
    new_string = my_string[:not_position] + \
        'good' + my_string[poor_position+4:]
else:
    new_string = my_string

print(new_string)

# solution 2
not_pos = my_string.find('not')
poor_pos = my_string.find('poor')

if not_pos < poor_pos and not_pos != -1:
    new_string = my_string.replace(my_string[not_pos:(poor_pos+4)], 'good')
else:
    new_string = my_string

print(new_string)

"""
8. Write a Python function that takes a list of words and returns
the length of the longest one.
"""

my_words = input("enter a few words ")

# solution 1
my_list = my_words.split()

max_len = 0
for word in my_list:
    if len(word) > max_len:
        max_len = len(word)

print(max_len)

# solution 2
my_list = my_words.split()
len_list = []
for word in my_list:
    len_list.append(len(word))

len_list.sort()
max_len = len_list[-1]
print(max_len)

"""
9.remove the nth index character
from a nonempty string.
"""

# solution 1
my_string = input("enter a string ")
n = int(input("enter a number "))
new_string = my_string[:n-1] + my_string[n:]
print(new_string)

"""
10.change a given string to a new string
where the first and last chars have been exchanged.
"""

my_string = input("enter a string ")

# soltuion 1
new_string = my_string[-1] + my_string[1:-1] + my_string[0]
print(new_string)

"""
11.remove the characters
which have odd index values of a given string.
"""

my_string = input("enter a string ")

# solution 1
my_list = [my_string[x] for x in range(len(my_string)) if x % 2 == 0]
new_string = ''.join(my_list)
print(new_string)

# solution 2
new_string = ''
for i in range(len(my_string)):
    if i % 2 == 0:
        new_string += my_string[i]
print(new_string)

"""
12.count the occurrences of each word
in a given sentence.
"""

my_string = input("enter a sentence ")

# solution 1
my_list = my_string.split()
my_set = set(my_list)
my_dict = {}
for word in my_set:
    my_dict[word] = my_string.count(word)
print(my_dict)

# solution 2
my_list = my_string.split()
my_set = set(my_list)
my_dict = {}
for word in my_set:
    my_dict[word] = 0
    for item in my_list:
        if word == item:
            my_dict[word] += 1
print(my_dict)

# solution 3
my_list = my_string.split()
my_dict = {}
for word in my_list:
    if word in my_dict:
        my_dict[word] += 1
    else:
        my_dict[word] = 1
print(my_dict)

"""
13. Write a Python script that takes input from the user
and displays that input back in upper and lower cases.
"""

my_string = input("enter something ")

print(my_string.upper())
print(my_string.lower())

"""
14. Write a Python program that accepts a comma separated sequence of words
as input and prints the unique words in sorted form (alphanumerically). 
Sample Words : red, white, black, red, green, black
Expected Result : black, green, red, white,red
"""

my_string = input("enter words ")
my_set = sorted(set(my_string.split(',')))
print(','.join(my_set))

"""
15. Write a Python function to create the HTML string with tags around the word(s).
Sample function and result : 
add_tags('i', 'Python') -> '<i>Python</i>'
add_tags('b', 'Python Tutorial') -> '<b>Python Tutorial </b>'
"""

# solution 1


def add_tags(tag, text):
    return "<"+tag+">"+text+"<"+tag+">"


print(add_tags('i', 'Python'))
print(add_tags('b', 'Python Tutorial'))

# solution 2


def add_tags(tag, text):
    return "<%s>%s<%s>" % (tag, text, tag)


print(add_tags('i', 'Python'))
print(add_tags('b', 'Python Tutorial'))

"""
16. Write a Python function to insert a string in the middle of a string. 
Sample function and result : 
insert_sting_middle('[[]]', 'Python') -> [[Python]]
insert_sting_middle('{{}}', 'PHP') -> {{PHP}}
"""


def insert_sting_middle(main_string, middle_string):
    main_len = len(main_string)
    middle = int(main_len/2)
    return main_string[:middle] + middle_string + main_string[middle:]


print(insert_sting_middle('[[]]', 'Python'))
print(insert_sting_middle('{{}}', 'PHP'))
print(insert_sting_middle('<<>>', 'HTML'))

"""
17. Write a Python function to get a string made of 4 copies of the
last two characters of a specified string (length must be at least 2). 
Sample function and result : 
insert_end('Python') -> onononon
insert_end('Exercises') -> eseseses
"""


def insert_end(my_string):
    return my_string[-2:] * 4


print(insert_end('Python'))
print(insert_end('Exercises'))

"""
18. Write a Python function to get a string made of its first three
characters of a specified string. If the length of the string is less
than 3 then return the original string. 
Sample function and result : 
first_three('ipy') -> ipy
first_three('python') -> pyt
"""


def first_three(my_string):
    if len(my_string) > 2:
        return my_string[:3]
    else:
        return my_string


print(first_three('ipy'))
print(first_three('python'))

"""
19.get the last part of a string
before a specified character. 
https://www.w3resource.com/python-exercises
https://www.w3resource.com/python
"""

# solution 1


def get_last_part(my_string, char):
    position = my_string.rfind(char)
    return my_string[:position]


print(get_last_part('https://www.w3resource.com/python', "/"))

# solution 2


def get_last_part(my_string, char):
    my_list = my_string.split(char)
    return char.join(my_list[:-1])


print(get_last_part('https://www.w3resource.com/python', "/"))

"""
20. Write a Python function to reverses a string if it's length is a multiple of 4.
"""

# solution 1


def reverse_string(my_string):
    if len(my_string) % 4 == 0:
        my_list = list(my_string)
        my_list.reverse()
        return ''.join(my_list)
    else:
        return my_string


my_string = input("enter a string ")
print(reverse_string(my_string))

# solution 2


def reverse_string(my_string):
    if len(my_string) % 4 == 0:
        return ''.join(reversed(my_string))
    else:
        return my_string


my_string = input("enter a string ")
print(reverse_string(my_string))

"""
21. Write a Python function to convert a given string to all uppercase
if it contains at least 2 uppercase characters in the first 4 characters.
"""


def to_upper(my_string):
    counter = 0
    for i in my_string[:4]:
        if i.isupper():
            counter += 1
    if counter > 1:
        return my_string.upper()
    else:
        return my_string


my_string = input("enter a string ")
print(to_upper(my_string))

"""
22.Write a Python program to sort a string lexicographically.
"""

my_string = input("enter a string ")
print(''.join(sorted(my_string, key=str.lower)))

"""
24.check whether a string starts with specified characters.
"""


def starts_with(my_string, char):
    if my_string.startswith(char):
        return True
    else:
        return False


my_string = input("enter a string ")
my_char = input("enter a character ")

print("does the string '%s' starts with the character '%s'?" %
      (my_string, my_char))
print(starts_with(my_string, my_char))

"""
25.create a Caesar encryption. 
Note : In cryptography, a Caesar cipher, also known as Caesar's cipher,
the shift cipher, Caesar's code or Caesar shift, is one of the simplest
and most widely known encryption techniques. It is a type of substitution
cipher in which each letter in the plaintext is replaced by a letter some
fixed number of positions down the alphabet. For example, with a left shift of 3,
D would be replaced by A, E would become B, and so on. The method is named
after Julius Caesar, who used it in his private correspondence.
"""


def Caesar_encryption(my_string, my_shift, my_side):
    # for decription need to use negative shift(step) or same shift opposite side
    step = my_shift
    shift_side = my_side
    new_string = ''
    for letter in my_string:
        letter_ord = ord(letter)
        if letter.islower():
            if shift_side == 'left':
                if (letter_ord-step) < ord('a'):
                    letter_ord += 26
                new_string += chr(letter_ord-step)
            elif shift_side == 'right':
                if (letter_ord+step) > ord('z'):
                    letter_ord -= 26
                new_string += chr(letter_ord+step)
        if letter.isupper():
            if shift_side == 'left':
                if (letter_ord-step) < ord('A'):
                    letter_ord += 26
                new_string += chr(letter_ord-step)
            elif shift_side == 'right':
                if (letter_ord+step) > ord('Z'):
                    letter_ord -= 26
                new_string += chr(letter_ord+step)
    return new_string


my_string = input("enter a string ")
my_shift = int(input("enter shift number "))
my_side = input("enter 'left' for left shift or 'right' for right shift ")
print("encrepted string is", Caesar_encryption(my_string, my_shift, my_side))

"""
30.print the following floating numbers
upto 2 decimal places.
"""

my_float = float(input("enter a float number "))

# solution 1
print("this is a float number %.2f" % my_float)

# solution 2
print("this is a float number {:.2f}".format(my_float))

"""
31.print the following floating numbers
upto 2 decimal places with a sign.
"""

my_float = float(input("enter a float number "))

# solution 1
print("this is a float number %+.2f" % my_float)

# solution 2
print("this is a float number {:+.2f}".format(my_float))

"""
32.print the following floating numbers
with no decimal places.
"""

my_float = float(input("enter a float number "))

# solution 1
print("this is a float number %.0f" % my_float)

# solution 2
print("this is a float number {:.0f}".format(my_float))

"""
33.print the following integers with zeros
on the left of specified width.
"""

my_string = input("enter an integer ")

# solution 1
print(my_string.zfill(5))

# solution 2
print('{:0>5s}'.format(my_string))

"""
34.print the following integers with '*'
on the right of specified width.
"""

my_string = input("enter an integer ")

print('{:*<5s}'.format(my_string))

"""
35.display a number with a comma separator.
"""

my_int = int(input("enter a number "))

print('{:,}'.format(my_int))

"""
36.format a number with a percentage
"""

my_num = float(input("enter a number "))

print('{:.2%}'.format(my_num))

"""
37.display a number in left,
right and center aligned of width 10.
"""

my_num = float(input("enter a number "))

print('{:<10}'.format(my_num))
print('{:>10}'.format(my_num))
print('{:^10}'.format(my_num))

"""
38.count occurrences of a substring in a string.
"""

my_string = input("enter a string ")
my_substring = input("enter a substring ")

print("'%s' occures in '%s' %d times" %
      (my_substring, my_string, my_string.count(my_substring)))

"""
39.reverse a string.
"""

my_string = input("enter a string ")

# solution 1
my_list = list(my_string)
my_list.reverse()
print(''.join(my_list))

# solution 2
print(''.join(reversed(my_string)))

"""
40.reverse words order in a string.
"""

my_sentence = input("enter a sentence ")

my_list = my_sentence.split()
print(' '.join(my_list[::-1]))

"""
41.strip a set of characters from a string.
"""

my_string = input("enter a website domain ")

# solution 1
print(my_string.strip('wcom.'))

# solution 2
my_list = [x for x in my_string if x not in 'wcom.']
# print(''.join(my_list))

"""
42. Write apython program to count repeated characters in a string. 
Sample string: 'thequickbrownfoxjumpsoverthelazydog'
Expected output :
o 4
e 3
u 2
h 2
r 2
t 2
"""

my_string = input("enter a string ")

# solution 1
my_list = []
for letter in my_string:
    my_list.append((letter, my_string.count(letter)))
my_unique_list = list(set(my_list))
for i in my_unique_list:
    print(i[0], i[1])

print('*' * 10)

# solution 2
my_dict = {}
for letter in my_string:
    if letter in my_dict:
        my_dict[letter] += 1
    else:
        my_dict[letter] = 1
for key, value in my_dict.items():
    print(key, value)

"""
43.print the square and cube symbol
in the area of a rectangle and volume of a cylinder. 
Sample output: 
The area of the rectangle is 1256.66cm2
The volume of the cylinder is 1254.725cm3
"""


def rectangle_area(length, width):
    return length*width


def cylinder_volume(r, height):
    return pi*r*r*height


# solution 1 using unicode superscript
print('The area of the rectangle is %.2f cm\u00B2' % rectangle_area(5.5, 7))
print('The volume of the cylinder is %.2f cm\u00B3' % cylinder_volume(5.5, 7))

"""
44.print the index of the character in a string. 
Sample string: w3resource
Expected output:
Current character w position at 0
Current character 3 position at 1
Current character r position at 2
- - - - - - - - - - - - - - - - - - - - - - - - -
Current character c position at 8
Current character e position at 9
"""

my_string = 'w3resource'

for i, v in enumerate(my_string):
    print("Current character %s position at %d" % (v, i))

"""
45.check if a string contains all letters
of the alphabet.
"""
my_string = input("enter a string ")

# solution 1
alphabet = 'abcdefghijklmnopqrstuvwxyz'
counter = 0
for letter in alphabet:
    if letter in my_string.lower():
        counter += 1
    else:
        break
if counter == 26:
    print('the string contains all the letters of the alphabet')
else:
    print('the string DOES NOT contain all the letters of the alphabet')

# solution 2
string_letters = ''.join(sorted(set(my_string.lower())))
print(string_letters)
if string.ascii_lowercase == string_letters:
    print('the string contains all the letters of the alphabet')
else:
    print('the string DOES NOT contain all the letters of the alphabet')

"""
47.lowercase first n characters in a string.
"""

my_string = input('enter a string ')
n = 3
print(my_string[:n].lower()+my_string[n:])

"""
48.swap comma and dot in a string. 
Sample string: "32.054,23"
Expected Output: "32,054.23"
"""

# solution 1
my_string = '32.054,23'
print(my_string)
my_list = my_string.split('.')
new_list = []
for i in my_list:
    new_list.append(i.replace(',', '.'))
new_string = ','.join(new_list)
print(new_string)

# solution 2
my_string = '32.054,23'
print(my_string)
swap = str.maketrans(',.', '.,')
new_string = my_string.translate(swap)
print(new_string)

"""
49.count and display the vowels of a given text.
"""

my_string = input("enter a text ")

# solution 1
vowels = 'aeiouy'
my_dict = {}
for i in vowels:
    my_dict[i] = 0
for i in my_string.lower():
    if i in vowels:
        my_dict[i] += 1
print(my_dict)

# solution 2
vowels = 'aeiouy'
my_string = my_string.lower()
my_list = [(x, my_string.count(x)) for x in my_string if x in vowels]
print(set(my_list))

"""
50.split a string on the last occurrence
of the delimiter.
"""

my_string = 'this is some text with space as a delimeter'
print(my_string.rsplit(maxsplit=1))

"""
51.find the first non-repeating character in given string.
"""


def first_non_repeating_character(str1):
    char_order = []
    ctr = {}
    for c in str1:
        if c in ctr:
            ctr[c] += 1
        else:
            ctr[c] = 1
            char_order.append(c)
    for c in char_order:
        if ctr[c] == 1:
            return c
    return None


print(first_non_repeating_character('abcdef'))
print(first_non_repeating_character('abcabcdef'))
print(first_non_repeating_character('aabbcc'))


# 52.print all permutations with given repetition number of characters of a given string. 
from itertools import product
def all_repeat(str1, rno):
  chars = list(str1)
  results = []
  for c in product(chars, repeat = rno):
    results.append(c)
  return results
print(all_repeat('xyz', 3))
print(all_repeat('xyz', 2))
print(all_repeat('abcd', 4))


# 53.find the first repeated character in a given string. 
def first_repeated_char(str1):
  for index,c in enumerate(str1):
    if str1[:index+1].count(c) > 1:
      return c 
  return "None"

print(first_repeated_char("abcdabcd"))
print(first_repeated_char("abcd"))


# 54.find the first repeated character of a given string where the index of first occurrence is smallest. 
def first_repeated_char_smallest_distance(str1):
  temp = {}
  for ch in str1:
    if ch in temp:
      return ch, str1.index(ch);
    else:
      temp[ch] = 0
  return 'None'
print(first_repeated_char_smallest_distance("abcabc"))
print(first_repeated_char_smallest_distance("abcb"))
print(first_repeated_char_smallest_distance("abcc"))
print(first_repeated_char_smallest_distance("abcxxy"))
print(first_repeated_char_smallest_distance("abc"))))


# 55.Write a Python program to find the first repeated word in a given string. 
def first_repeated_char_smallest_distance(str1):
  temp = {}
  for ch in str1:
    if ch in temp:
      return ch, str1.index(ch);
    else:
      temp[ch] = 0
  return 'None'
print(first_repeated_char_smallest_distance("abcabc"))
print(first_repeated_char_smallest_distance("abcb"))
print(first_repeated_char_smallest_distance("abcc"))
print(first_repeated_char_smallest_distance("abcxxy"))
print(first_repeated_char_smallest_distance("abc"))))


# 56.find the second most repeated word in a given string. 
def word_count(str):
    counts = dict()
    words = str.split()

    for word in words:
        if word in counts:
            counts[word] += 1
        else:
            counts[word] = 1

    counts_x = sorted(counts.items(), key=lambda kv: kv[1])
    #print(counts_x)
    return counts_x[-2]
 
print(word_count("Both of these issues are fixed by postponing the evaluation of annotations. Instead of compiling code which executes expressions in annotations at their definition time, the compiler stores the annotation in a string form equivalent to the AST of the expression in question. If needed, annotations can be resolved at runtime using typing.get_type_hints(). In the common case where this is not required, the annotations are cheaper to store (since short strings are interned by the interpreter) and make startup time faster."))


# 57.Write a Python program to remove spaces from a given string. 
def remove_spaces(str1):
  str1 = str1.replace(' ','')
  return str1
    
print(remove_spaces("w 3 res ou r ce"))
print(remove_spaces("a b c"))


# 58.move spaces to the front of a given string. 
def move_Spaces_front(str1):
  noSpaces_char = [ch for ch in str1 if ch!=' ']
  spaces_char = len(str1) - len(noSpaces_char)
  result = ' '*spaces_char
  result = '"'+result + ''.join(noSpaces_char)+'"'
  return(result)

print(move_Spaces_front("w3resource .  com  "))
print(move_Spaces_front("   w3resource.com  "))


# 59.find the maximum occurring character in a given string. 
def get_max_occuring_char(str1):
  ASCII_SIZE = 256
  ctr = [0] * ASCII_SIZE
  max = -1
  ch = ''
  for i in str1:
    ctr[ord(i)]+=1;
 
  for i in str1:
    if max < ctr[ord(i)]:
      max = ctr[ord(i)]
      ch = i
  return ch

print(get_max_occuring_char("Python: Get file creation and modification date/times"))
print(get_max_occuring_char("abcdefghijkb"))


# 60.capitalize first and last letters of each word of a given string. 
def capitalize_first_last_letters(str1):
     str1 = result = str1.title()
     result =  ""
     for word in str1.split():
        result += word[:-1] + word[-1].upper() + " "
     return result[:-1]  
     
print(capitalize_first_last_letters("python exercises practice solution"))
print(capitalize_first_last_letters("w3resource"))


# 61.remove duplicate characters of a given string. 
from collections import OrderedDict
def remove_duplicate(str1):
  return "".join(OrderedDict.fromkeys(str1))
     
print(remove_duplicate("python exercises practice solution"))
print(remove_duplicate("w3resource"))


# 62.compute sum of digits of a given string. 
def sum_digits_string(str1):
    sum_digit = 0
    for x in str1:
        if x.isdigit() == True:
            z = int(x)
            sum_digit = sum_digit + z

    return sum_digit
     
print(sum_digits_string("123abcd45"))
print(sum_digits_string("abcd1234"))


# 63.remove leading zeros from an IP address. 
def remove_zeros_from_ip(ip_add):
  new_ip_add = ".".join([str(int(i)) for i in ip_add.split(".")])  
  return new_ip_add ;

print(remove_zeros_from_ip("255.024.01.01"))
print(remove_zeros_from_ip("127.0.0.01 "))


# 64.find maximum length of consecutive 0's in a given binary string. 
def max_consecutive_0(input_str): 
     return  max(map(len,input_str.split('1')))
str1 = '111000010000110'
print("Original string:" + str1)
print("Maximum length of consecutive 0’s:")
print(max_consecutive_0(str1))
str1 = '111000111'
print("Original string:" + str1)
print("Maximum length of consecutive 0’s:")
print(max_consecutive_0(str1))


# 65.find all the common characters in lexicographical order from two given lower case strings. If there are no common letters print "No common characters". 
from collections import Counter 
def common_chars(str1,str2): 	
	d1 = Counter(str1) 
	d2 = Counter(str2) 
	common_dict = d1 & d2 
	if len(common_dict) == 0: 
		return "No common characters."

	# list of common elements 
	common_chars = list(common_dict.elements()) 
	common_chars = sorted(common_chars) 

	return ''.join(common_chars) 

str1 = 'Python'
str2 = 'PHP'
print("Two strings: "+str1+' : '+str2)
print(common_chars(str1, str2))
str1 = 'Java'
str2 = 'PHP'
print("Two strings: "+str1+' : '+str2)
print(common_chars(str1, str2))


# 66.make two given strings (lower case, may or may not be of the same length) anagrams removing any characters from any of the strings. 
def make_map(s):
    temp_map = {}
    for char in s:
        if char not in temp_map:
            temp_map[char] = 1
        else:
            temp_map[char] +=1 
    return temp_map        
def make_anagram(str1, str2):
    str1_map1 = make_map(str1)
    str2_map2 = make_map(str2)
 
    ctr = 0
    for key in str2_map2.keys():
        if key not in str1_map1:
            ctr += str2_map2[key]
        else:
            ctr += max(0, str2_map2[key]-str1_map1[key])
 
    for key in str1_map1.keys():
        if key not in str2_map2:
            ctr += str1_map1[key]
        else:
            ctr += max(0, str1_map1[key]-str2_map2[key]) 
    return ctr 
str1 = input("Input string1: ")
str2 = input("Input string2: ")
print(make_anagram(str1, str2))


# 67.remove all consecutive duplicates of a given string. 
from itertools import groupby 
def remove_all_consecutive(str1): 
	result_str = [] 
	for (key,group) in groupby(str1): 
		result_str.append(key) 

	return ''.join(result_str)
	
str1 = 'xxxxxyyyyy'
print("Original string:" + str1)
print("After removing consecutive duplicates: " + str1)
print(remove_all_consecutive(str1))


# 68.create two strings from a given string. Create the first string using those character which occurs only once and create the second string which consists of multi-time occurring characters in the said string. 
from collections import Counter  
def generateStrings(input): 
     str_char_ctr = Counter(input) 
     part1 = [ key for (key,count) in str_char_ctr.items() if count==1] 
     part2 = [ key for (key,count) in str_char_ctr.items() if count>1] 
     part1.sort() 
     part2.sort()
     return part1,part2
input = "aabbcceffgh"
s1, s2 = generateStrings(input)
print(''.join(s1))   
print(''.join(s2))


# 69.find the longest common sub-string from two given strings. 
from difflib import SequenceMatcher 
  
def longest_Substring(s1,s2): 
  
     seq_match = SequenceMatcher(None,s1,s2) 
  
     match = seq_match.find_longest_match(0, len(s1), 0, len(s2)) 
  
     # return the longest substring 
     if (match.size!=0): 
          return (s1[match.a: match.a + match.size])  
     else: 
          return ('Longest common sub-string not present')  

s1 = 'abcdefgh'
s2 = 'xswerabcdwd'
print("Original Substrings:\n",s1+"\n",s2)
print("\nCommon longest sub_string:")
print(longest_Substring(s1,s2))


# 70.create a string from two given strings concatenating uncommon characters of the said strings. 
def uncommon_chars_concat(s1, s2):   
     
     set1 = set(s1) 
     set2 = set(s2) 
  
     common_chars = list(set1 & set2) 
     result = [ch for ch in s1 if ch not in common_chars] + [ch for ch in s2 if ch not in common_chars] 
     return(''.join(result))

s1 = 'abcdpqr'
s2 = 'xyzabcd'
print("Original Substrings:\n",s1+"\n",s2)
print("\nAfter concatenating uncommon characters:")
print(uncommon_chars_concat(s1, s2))


# 71.move all spaces to the front of a given string in single traversal. 
def moveSpaces(str1): 
    no_spaces = [char for char in str1 if char!=' ']   
    space= len(str1) - len(no_spaces)
    # Create string with spaces
    result = ' '*space    
    return result + ''.join(no_spaces)
  
s1 = "Python Exercises"
print("Original String:\n",s1)

print("\nAfter moving all spaces to the front:")
print(moveSpaces(s1))


# 72. remove all characters except a specified character in a given string. 
# Original string
# Python Exercises
# Remove all characters except P in the said string:
# P
# Original string
# google
# Remove all characters except g in the said string:
# gg
# Original string
# exercises
# Remove all characters except e in the said string:
# eee
def remove_characters(str1,c):
    return ''.join([el for el in str1 if el == c])
text = "Python Exercises"
print("Original string")
print(text)
except_char = "P"
print("Remove all characters except",except_char,"in the said string:")
print(remove_characters(text,except_char))
text = "google"
print("\nOriginal string")
print(text)
except_char = "g"
print("Remove all characters except",except_char,"in the said string:")
print(remove_characters(text,except_char))
text = "exercises"
print("\nOriginal string")
print(text)
except_char = "e"
print("Remove all characters except",except_char,"in the said string:")
print(remove_characters(text,except_char))


# 73.count Uppercase, Lowercase, special character and numeric values in a given string. 
def count_chars(str):
     upper_ctr, lower_ctr, number_ctr, special_ctr = 0, 0, 0, 0
     for i in range(len(str)):
          if str[i] >= 'A' and str[i] <= 'Z': upper_ctr += 1
          elif str[i] >= 'a' and str[i] <= 'z': lower_ctr += 1
          elif str[i] >= '0' and str[i] <= '9': number_ctr += 1
          else: special_ctr += 1
     return upper_ctr, lower_ctr, number_ctr, special_ctr
           
str = "@W3Resource.Com"
print("Original Substrings:",str)
u, l, n, s = count_chars(str)
print('\nUpper case characters: ',u)
print('Lower case characters: ',l)
print('Number case: ',n)
print('Special case characters: ',s)


# 74.find the minimum window in a given string which will contain all the characters of another given string. 
# Example 1
# Input : str1 = " PRWSOERIUSFK "
# str2 = " OSU "
# Output: Minimum window is "OERIUS"
import collections
def min_window(str1, str2):
    result_char, missing_char = collections.Counter(str2), len(str2)
    i = p = q = 0
    for j, c in enumerate(str1, 1):
        missing_char -= result_char[c] > 0
        result_char[c] -= 1
        if not missing_char:
            while i < q and result_char[str1[i]] < 0:
                result_char[str1[i]] += 1
                i += 1
            if not q or j - i <= q - p:
                p, q = i, j
    return str1[p:q]
           
str1 = "PRWSOERIUSFK"
str2 = "OSU"
print("Original Strings:\n",str1,"\n",str2)
print("Minimum window:")
print(min_window(str1,str2))


# 75.find smallest window that contains all characters of a given string. 
from collections import defaultdict   

def find_sub_string(str): 
    str_len = len(str) 
      
    # Count all distinct characters. 
    dist_count_char = len(set([x for x in str])) 
  
    ctr, start_pos, start_pos_index, min_len = 0, 0, -1, 9999999999
    curr_count = defaultdict(lambda: 0) 
    for i in range(str_len): 
        curr_count[str[i]] += 1
 
        if curr_count[str[i]] == 1: 
            ctr += 1
  
        if ctr == dist_count_char: 
            while curr_count[str[start_pos]] > 1: 
                if curr_count[str[start_pos]] > 1: 
                    curr_count[str[start_pos]] -= 1
                start_pos += 1
  
            len_window = i - start_pos + 1
            if min_len > len_window: 
                min_len = len_window 
                start_pos_index = start_pos 
    return str[start_pos_index: start_pos_index + min_len] 
      
str1 = "asdaewsqgtwwsa"
print("Original Strings:\n",str1)
print("\nSmallest window that contains all characters of the said string:")
print(find_sub_string(str1)) 


# 76.count number of substrings from a given string of lowercase alphabets with exactly k distinct (given) characters. 
def count_k_dist(str1, k): 
	str_len = len(str1) 
	
	result = 0

	ctr = [0] * 27

	for i in range(0, str_len): 
		dist_ctr = 0

		ctr = [0] * 27

		for j in range(i, str_len): 
			
			if(ctr[ord(str1[j]) - 97] == 0): 
				dist_ctr += 1

			ctr[ord(str1[j]) - 97] += 1

			if(dist_ctr == k): 
				result += 1
			if(dist_ctr > k): 
				break

	return result 

str1 = input("Input a string (lowercase alphabets):")
k = int(input("Input k: "))
print("Number of substrings with exactly", k, "distinct characters : ", end = "") 
print(count_k_dist(str1, k))


# 77.count number of non-empty substrings of a given string. 
def number_of_substrings(str): 
	str_len = len(str); 
	return int(str_len * (str_len + 1) / 2); 

str1 = input("Input a string: ")
print("Number of substrings:") 
print(number_of_substrings(str1))


# 78.count characters at same position in a given string (lower and uppercase characters) as in English alphabet. 
def count_char_position(str1): 
    count_chars = 0
    for i in range(len(str1)):
        if ((i == ord(str1[i]) - ord('A')) or 
            (i == ord(str1[i]) - ord('a'))): 
            count_chars += 1
    return count_chars 
  
str1 = input("Input a string: ")
print("Number of characters of the said string at same position as in English alphabet:")
print(count_char_position(str1))


# 79.find smallest and largest word in a given string. 
def smallest_largest_words(str1):
    word = "";
    all_words = [];
    str1 = str1 + " ";
    for i in range(0, len(str1)):
        if(str1[i] != ' '):
            word = word + str1[i];  
        else:
            all_words.append(word);  
            word = "";  
          
    small = large = all_words[0];  
   
#Find smallest and largest word in the str1  
    for k in range(0, len(all_words)):
        if(len(small) > len(all_words[k])):
            small = all_words[k];
        if(len(large) < len(all_words[k])):
            large = all_words[k];
    return small,large;

str1 = "Write a Java program to sort an array of given integers using Quick sort Algorithm.";  
print("Original Strings:\n",str1)
small, large = smallest_largest_words(str1)  
print("Smallest word: " + small);  
print("Largest word: " + large); 


# 80.count number of substrings with same first and last characters of a given string. 
def no_of_substring_with_equalEnds(str1): 
	result = 0; 
	n = len(str1); 
	for i in range(n): 
		for j in range(i, n): 
			if (str1[i] == str1[j]): 
				result = result + 1
	return result 
str1 = input("Input a string: ")
print(no_of_substring_with_equalEnds(str1))


# 81.find the index of a given string at which a given substring starts. If the substring is not found in the given string return 'Not found'. 
def find_Index(str1, pos):
    if len(pos) > len(str1):
        return 'Not found'

    for i in range(len(str1)):

        for j in range(len(pos)):

            if str1[i + j] == pos[j] and j == len(pos) - 1:
                return i
                
            elif str1[i + j] != pos[j]:
                break

    return 'Not found'
print(find_Index("Python Exercises", "Ex"))
print(find_Index("Python Exercises", "yt"))
print(find_Index("Python Exercises", "PY"))


# 82.wrap a given string into a paragraph of given width. 
# Sample Output:
# Input a string: The quick brown fox.
# Input the width of the paragraph: 10
# Result:
# The quick
# brown fox.
import textwrap
s = input("Input a string: ")
w = int(input("Input the width of the paragraph: ").strip())
print("Result:")
print(textwrap.fill(s,w))


# 83.print four values decimal, octal, hexadecimal (capitalized), binary in a single line of a given integer. 
# Sample Output:
# Input an integer: 25
# Decimal Octal Hexadecimal (capitalized), Binary
# 25 31 19 11001
i = int(input("Input an integer: "))
o = str(oct(i))[2:]
h = str(hex(i))[2:]
h = h.upper()
b = str(bin(i))[2:]
d = str(i)
print(f"Decimal {d} Octal {o} Hexadecimal (capitalized) {h}, Binary {b}")




# 84.swap cases of a given string. 
# Sample Output:
# pYTHON eXERCISES
# jAVA
# nUMpY
def swap_case_string(str1):
   result_str = ""   
   for item in str1:
       if item.isupper():
           result_str += item.lower()
       else:
           result_str += item.upper()           
   return result_str
print(swap_case_string("Python Exercises"))
print(swap_case_string("Java"))
print(swap_case_string("NumPy"))


# 85.convert a given Bytearray to Hexadecimal string. 
# Sample Output:
# Original Bytearray :
# [111, 12, 45, 67, 109]
# Hexadecimal string:
# 6f0c2d436d

def bytearray_to_hexadecimal(list_val):
     result = ''.join('{:02x}'.format(x) for x in list_val)  
     return(result)

list_val = [111, 12, 45, 67, 109] 
print("Original Bytearray :")
print(list_val)
print("\nHexadecimal string:")
print(bytearray_to_hexadecimal(list_val))

# 86.delete all occurrences of a specified character in a given string. 
# Sample Output:
# Original string:
# Delete all occurrences of a specified character in a given string
# Modified string:
# Delete ll occurrences of specified chrcter in given string
def delete_all_occurrences(str1, ch):
     result = str1.replace(ch, "")
     return(result)

str_text = "Delete all occurrences of a specified character in a given string"
print("Original string:")
print(str_text)
print("\nModified string:")
ch='a'
print(delete_all_occurrences(str_text, ch))


# 87. Write a Python program find the common values that appear in two given strings. 
# Sample Output:
# Original strings:
# Python3
# Python2.7
# Intersection of two said String:
# Python
def intersection_of_two_string(str1, str2):
    result = ""
    for ch in str1:
        if ch in str2 and not ch in result:
            result += ch
    return result

str1 = 'Python3'
str2 = 'Python2.7'
print("Original strings:")
print(str1)
print(str2)
print("\nIntersection of two said String:") 
print(intersection_of_two_string(str1, str2))


# 88.check whether a given string contains a capital letter, a lower case letter, a number and a minimum length. 
# Sample Output:
# Input the string: W3resource
# ['Valid string.']
def check_string(s):
    messg = []
    if not any(x.isupper() for x in s):
        messg.append('String must have 1 upper case character.')
    if not any(x.islower() for x in s):
        messg.append('String must have 1 lower case character.')
    if not any(x.isdigit() for x in s):
        messg.append('String must have 1 number.')
    if len(s) < 8:
        messg.append('String length should be atleast 8.')    
    if not messg:
        messg.append('Valid string.')
    return messg
    
s = input("Input the string: ")
print(check_string(s))


# 89.remove unwanted characters from a given string. 
# Sample Output:
# Original String : Pyth*^on Exercis^es
# After removing unwanted characters:
# Python Exercises
# Original String : A%^!B#*CD
# After removing unwanted characters:
# ABCD
def remove_chars(str1, unwanted_chars):
    for i in unwanted_chars:
        str1 = str1.replace(i, '')
    return str1



str1 = "Pyth*^on Exercis^es"
str2 = "A%^!B#*CD"

unwanted_chars = ["#", "*", "!", "^", "%"]
print ("Original String : " + str1)
print("After removing unwanted characters:")
print(remove_chars(str1, unwanted_chars))
print ("\nOriginal String : " + str2)
print("After removing unwanted characters:")
print(remove_chars(str2, unwanted_chars))


# 90.remove duplicate words from a given string. 
# Sample Output:
# Original String:
# Python Exercises Practice Solution Exercises
# After removing duplicate words from the said string:
# Python Exercises Practice Solution
def unique_list(text_str):
    l = text_str.split()
    temp = []
    for x in l:
        if x not in temp:
            temp.append(x)
    return ' '.join(temp)

text_str = "Python Exercises Practice Solution Exercises"
print("Original String:")
print(text_str)
print("\nAfter removing duplicate words from the said string:")
print(unique_list(text_str))


# 91.convert a given heterogeneous list of scalars into a string. 
# Sample Output:
# Original list:
# ['Red', 100, -50, 'green', 'w,3,r', 12.12, False]
# Convert the heterogeneous list of scalars into a string:
# Red,100,-50,green,w,3,r,12.12,False
def heterogeneous_list_to_str(lst):
    result = ','.join(str(x) for x in lst)
    return result
h_data = ["Red", 100, -50, "green", "w,3,r", 12.12, False]
print("Original list:")
print(h_data)
print("\nConvert the heterogeneous list of scalars into a string:")
print(heterogeneous_list_to_str(h_data))


# 92.find the string similarity between two given strings. 
# Sample Output:
# Original string:
# Python Exercises
# Python Exercises
# Similarity between two said strings:
# 1.0
# Original string:
# Python Exercises
# Python Exercise
# Similarity between two said strings:
# 0.967741935483871
# Original string:
# Python Exercises
# Python Ex.
# Similarity between two said strings:
# 0.6923076923076923
# Original string:
# Python Exercises
# Python
# Similarity between two said strings:
# 0.5454545454545454
# Original string:
# Java Exercises
# Python
# Similarity between two said strings:
# 0.0
import difflib
def string_similarity(str1, str2):
    result =  difflib.SequenceMatcher(a=str1.lower(), b=str2.lower())
    return result.ratio()
str1 = 'Python Exercises'
str2 = 'Python Exercises'
print("Original string:")
print(str1)
print(str2)
print("Similarity between two said strings:")
print(string_similarity(str1,str2))
str1 = 'Python Exercises'
str2 = 'Python Exercise'
print("\nOriginal string:")
print(str1)
print(str2)
print("Similarity between two said strings:")
print(string_similarity(str1,str2))
str1 = 'Python Exercises'
str2 = 'Python Ex.'
print("\nOriginal string:")
print(str1)
print(str2)
print("Similarity between two said strings:")
print(string_similarity(str1,str2))
str1 = 'Python Exercises'
str2 = 'Python'
print("\nOriginal string:")
print(str1)
print(str2)
print("Similarity between two said strings:")
print(string_similarity(str1,str2))
str1 = 'Python Exercises'
str1 = 'Java Exercises'
print("\nOriginal string:")
print(str1)
print(str2)
print("Similarity between two said strings:")
print(string_similarity(str1,str2))


# 93.extract numbers from a given string. 
# Sample Output:
# Original string: red 12 black 45 green
# Extract numbers from the said string: [12, 45]
def test(str1):
    result = [int(str1) for str1 in str1.split() if str1.isdigit()]
    return result
str1 = "red 12 black 45 green" 
print("Original string:", str1) 
print("Extract numbers from the said string:")
print(test(str1))


# 94.convert a hexadecimal color code to a tuple of integers corresponding to its RGB components. 
# Sample Output:
# (255, 165, 1)
# (255, 255, 255)
# (0, 0, 0)
# (255, 0, 0)
# (0, 0, 128)
# (192, 192, 192)
def hex_to_rgb(hex):
  return tuple(int(hex[i:i+2], 16) for i in (0, 2, 4)) 
print(hex_to_rgb('FFA501'))
print(hex_to_rgb('FFFFFF'))
print(hex_to_rgb('000000'))
print(hex_to_rgb('FF0000'))
print(hex_to_rgb('000080'))
print(hex_to_rgb('C0C0C0'))


# 95.convert the values of RGB components to a hexadecimal color code. 
# Sample Output:
# FFA501
# FFFFFF
# 000000
# 000080
# C0C0C0
def rgb_to_hex(r, g, b):
  return ('{:02X}' * 3).format(r, g, b)
 
print(rgb_to_hex(255, 165, 1))
print(rgb_to_hex(255, 255, 255))
print(rgb_to_hex(0, 0, 0))
print(rgb_to_hex(0, 0, 128))
print(rgb_to_hex(192, 192, 192))


# 96.convert a given string to camelcase. Use re.sub() to replace any - or _ with a space, using the regexp r"(_|-)+".
# Use str.title() to capitalize the first letter of each word and convert the rest to lowercase.
# Finally, use str.replace() to remove spaces between words.
# Sample Output:
# javascript
# fooBar
# fooBar
# foo.Bar
# fooBar
# foobar
# fooBar
from re import sub

def camel_case(s):
  s = sub(r"(_|-)+", " ", s).title().replace(" ", "")
  return ''.join([s[0].lower(), s[1:]])
print(camel_case('JavaScript'))
print(camel_case('Foo-Bar'))
print(camel_case('foo_bar'))
print(camel_case('--foo.bar'))
print(camel_case('Foo-BAR'))
print(camel_case('fooBAR'))
print(camel_case('foo bar'))


# 97.convert a given string to snake case. 
# Sample Output:
# java_script
# foo_bar
# foo_bar
# foo.bar
# foo_bar
# foo_bar
# foo_bar
from re import sub
def snake_case(s):
  return '_'.join(
    sub('([A-Z][a-z]+)', r' \1',
    sub('([A-Z]+)', r' \1',
    s.replace('-', ' '))).split()).lower()

print(snake_case('JavaScript'))
print(snake_case('Foo-Bar'))
print(snake_case('foo_bar'))
print(snake_case('--foo.bar'))
print(snake_case('Foo-BAR'))
print(snake_case('fooBAR'))
print(snake_case('foo bar'))


# 98.decapitalize the first letter of a given string. 
# Sample Output:
# java Script
# python
def decapitalize_first_letter(s, upper_rest = False):
  return ''.join([s[:1].lower(), (s[1:].upper() if upper_rest else s[1:])]) 
print(decapitalize_first_letter('Java Script'))
print(decapitalize_first_letter('Python'))


# 99.split a given multiline string into a list of lines. 
# Sample Output:
# Original string: This
# is a
# multiline
# string.
# Split the said multiline string into a list of lines:
# ['This', 'is a', 'multiline', 'string.', '']
def split_lines(s):
  return s.split('\n')
print("Original string:")
print("This\nis a\nmultiline\nstring.\n")
print("Split the said multiline string into a list of lines:")
print(split_lines('This\nis a\nmultiline\nstring.\n'))


# 100.check whether any word in a given sting contains duplicate characters or not. Return True or False. 
# Sample Output:
# Original text:
# Filter out the factorials of the said list.
# Check whether any word in the said sting contains duplicate characters or not!
# False
# Original text:
# Python Exercise.
# Check whether any word in the said sting contains duplicate characters or not!
# False
# Original text:
# The wait is over.
# Check whether any word in the said sting contains duplicate characrters or not!
# True
def duplicate_letters(text):
	word_list = text.split()
	for word in word_list:
		if len(word) > len(set(word)):
			return False
	return True
text = "Filter out the factorials of the said list."
print("Original text:")
print(text)
print("Check whether any word in the said sting contains duplicate characrters or not!")
print(duplicate_letters(text))
text = "Python Exercise."
print("\nOriginal text:")
print(text)
print("Check whether any word in the said sting contains duplicate characrters or not!")
print(duplicate_letters(text))
text = "The wait is over."
print("\nOriginal text:")
print(text)
print("Check whether any word in the said sting contains duplicate characrters or not!")
print(duplicate_letters(text))


# 101.add two strings as they are numbers (Positive integer values). Return a message if the numbers are string. 
# Sample Output:
# 42
# Error in input!
# Error in input!
def test(n1, n2):
    n1, n2 = '0' + n1, '0' + n2
    if (n1.isnumeric() and n2.isnumeric()):
        return str(int(n1) + int(n2))
    else:
        return 'Error in input!'
print(test("10", "32"))
print(test("10", "22.6"))
print(test("100", "-200")) 




Tuple.py """
1. Write a Python program to create a tuple.
"""


my_empty_tuple = ()
my_another_empty_tuple = tuple()
my_single_tuple = 'abc',
my_multiple_tuple = 'abc', 'bcd', 'xyz'
my_multiple_tuple_with_braktes = ('abc', 'bcd', 'xyz')
my_list = [1, 2, 3, 3, 4]
my_tuple_from_list = tuple(my_list)


print(my_empty_tuple, type(my_empty_tuple), '\n',
      my_another_empty_tuple, type(my_another_empty_tuple), '\n',
      my_single_tuple, type(my_single_tuple), '\n',
      my_multiple_tuple, type(my_multiple_tuple), '\n',
      my_multiple_tuple_with_braktes, type(my_multiple_tuple_with_braktes), '\n',
      my_tuple_from_list, type(my_tuple_from_list))

"""
2. Write a Python program to create a tuple with different data types.
"""

my_tuple = ('abc', 1, 4.56, ['a', 'b', 'c'], True)
print(my_tuple)

for i in my_tuple:
    print(i, 'type is', type(i))

"""
3. Write a Python program to create a tuple with numbers and print one item.
"""
from random import randint
my_tuple = (1, 2, 3, 3, 4)
print(my_tuple[randint(0, len(my_tuple)-1)])

"""
4. Write a Python program to unpack a tuple in several variables.
"""
my_tuple = ('a', 'b')
var1, var2 = my_tuple
print(var1, var2)

"""
5. Write a Python program to add an item in a tuple.
"""

my_tuple = ('a', 'b')
my_new_tuple = my_tuple + ('c',)

print(my_new_tuple)

"""
6. Write a Python program to convert a tuple to a string.
"""
my_tuple = ('a', 'b')
my_string = ''.join(my_tuple)
print(my_string)

"""
7. Write a Python program to get the 4th element
and 4th element from last of a tuple.
"""

my_tuple = tuple('abcdefgh')
print(my_tuple)
print('4th element is', my_tuple[3])
print('4th to last element is', my_tuple[-4]) 

"""
8. Write a Python program to create the colon of a tuple.
"""

my_tuple = (':',)
my_colon, = my_tuple
print(my_colon)

"""
9. Write a Python program to find the repeated items of a tuple.
"""

my_tuple = (1, 1, 2, 3, 4, 4, 5)

repeated_items = [i for i in my_tuple if my_tuple.count(i) > 1]
print(set(repeated_items))

"""
10. Write a Python program to check whether an element exists within a tuple.
"""

my_tuple = (1, 2, 3, 4, 5)

if 2 in my_tuple:
    print('2 is in', my_tuple)
else:
    print('2 is NOT in', my_tuple)

if 6 in my_tuple:
    print('6 is in', my_tuple)
else:
    print('6 is NOT in', my_tuple)

"""
11. Write a Python program to convert a list to a tuple.
"""

my_list = [1, 2, 3, 4, 5]
my_tuple = tuple(my_list)
print(my_tuple)

"""
12. Write a Python program to remove an item from a tuple.
"""

my_tuple = (1, 2, 3, 4, 5)
# to remove number 3
my_new_tuple = my_tuple[:my_tuple.index(3)] + my_tuple[my_tuple.index(3)+1:]
print(my_new_tuple)

"""
13. Write a Python program to slice a tuple.
"""

my_tuple = tuple('axbxcx')

print(my_tuple[::2])

"""
14. Write a Python program to find the index of an item of a tuple.
"""

my_tuple = ('a', 'b')
print(my_tuple.index('a'))

"""
15. Write a Python program to find the length of a tuple.
"""

my_tuple = ('a', 'b')
print(len(my_tuple))

"""
16. Write a Python program to convert a tuple to a dictionary.
"""

my_tuple = ('a', 1, 'b', 2)

my_dict = {my_tuple[i]: my_tuple[i+1] for i in range(0, len(my_tuple), 2)}
print(my_dict)

"""
17. Write a Python program to unzip a list of tuples into individual lists.
"""

my_list = [(1, 2), ('a', 'b'), (True, False)]
new_list = [list(i) for i in my_list]
print(*new_list)

"""
18. Write a Python program to reverse a tuple.
"""
my_tuple = (1, 2, 3, 4)
reversed_tuple = my_tuple[::-1]
print(reversed_tuple)

"""
19. Write a Python program to convert a list of tuples into a dictionary.
"""

# solution 1 for unique keys
my_list = [('a', 1), ('b', 2)]
my_dict = {i[0]:i[1] for i in my_list}
print(my_dict)

# solution 2 for same key appearing in several tuples
my_list = [('a', 1), ('b', 2), ('a', 5)]
my_dict = {}
for i in my_list:
    my_dict.setdefault(i[0],[]).append(i[1])
print(my_dict)

"""
20. Write a Python program to print a tuple with string formatting.
Sample tuple : (100, 200, 300)
Output : This is a tuple (100, 200, 300)
"""

my_tuple = (100, 200, 300)
print(f'This is a tuple {my_tuple}')

"""
21. Write a Python program to replace last value of tuples in a list.
Sample list: [(10, 20, 40), (40, 50, 60), (70, 80, 90)]
Expected Output: [(10, 20, 100), (40, 50, 100), (70, 80, 100)]
"""

my_list = [(10, 20, 40), (40, 50, 60), (70, 80, 90)]

new_list = [i[:-1] + (100,) for i in my_list]
print(new_list)

"""
22. Write a Python program to remove an empty tuple(s) from a list of tuples.
Sample data: [(), (), ('',), ('a', 'b'), ('a', 'b', 'c'), ('d')]
Expected output: [('',), ('a', 'b'), ('a', 'b', 'c'), 'd']
"""


my_list = [(), (), ('',), ('a', 'b'), ('a', 'b', 'c'), ('d')]

new_list = [i for i in my_list if i]
print(new_list)

"""
23. Write a Python program to sort a listof tuples by the float element.
Sample data: [('item1', '12.20'), ('item2', '15.10'), ('item3', '24.5')]
Expected Output: [('item3', '24.5'), ('item2', '15.10'), ('item1', '12.20')]
"""

my_list = [('item1', '12.20'), ('item2', '15.10'), ('item3', '24.5')]

my_list.sort(key=lambda x: x[1], reverse=True)
print(my_list)

"""
24. Write a Python program to count the elements in a list until an element is a tuple.
"""

my_list = [1, 2, 3, (4,), 5, 6]

counter = 0
for i in my_list:
    if not isinstance(i, tuple):
        counter += 1
    else:
        break
print(counter)




WebScraping.py # 1. Write a Python program to test if a given page is found or not on the server. 
import csv
from bs4 import BeautifulSoup
from lxml import html
from urllib.request import urlopen
from urllib.error import HTTPError
from urllib.error import URLError

try:
    html = urlopen("https://abcxyz.com")
except HTTPError as e:
    print("HTTP error")
except URLError as e:
    print("Server not found!")
else:
    print(html.read())
    
try:
    html = urlopen("http://www.example.com/")
except HTTPError as e:
    print("HTTP error")
except URLError as e:
    print("Server not found!")
else:
    print("HTML Details")    
    print(html.read())  

# 2. Write a Python program to download and display the content of robot.txt for en.wikipedia.org. 
import requests
response = requests.get("https://en.wikipedia.org/robots.txt")
test = response.text
print("robots.txt for http://www.wikipedia.org/")
print("===================================================")
print(test)

# 3. Write a Python program to get the number of datasets currently listed on data.gov.
response = requests.get('http://www.data.gov/')
doc_gov = html.fromstring(response.text)
link_gov = doc_gov.cssselect('small a')[0]
print("Number of datasets currently listed on data.gov:")
print(link_gov.text)

# 4. Write a Python program to convert an address(like "1600 Amphitheatre Parkway, Mountain View, CA") into geographic coordinates(like latitude 37.423021 and longitude - 122.083739). 
# Geocodin: Geocoding is the process of converting addresses(like "1600 Amphitheatre Parkway, Mountain View, CA") into geographic coordinates(like latitude 37.423021 and longitude - 122.083739), which you can use to place markers on a map, or position the map.
geo_url = 'http://maps.googleapis.com/maps/api/geocode/json'
my_address = {'address': '21 Ramkrishana Road, Burdwan, East Burdwan, West Bengal, India',
              'language': 'en'}
response = requests.get(geo_url, params=my_address)
results = response.json()['results']
my_geo = results[0]['geometry']['location']
print("Longitude:", my_geo['lng'], "\n", "Latitude:", my_geo['lat'])

# 5. Write a Python program to display the name of the most recently added dataset on data.gov. 
from lxml import html
import requests
response = requests.get('http://catalog.data.gov/dataset?q=&sort=metadata_created+desc')
doc = html.fromstring(response.text)
title = doc.cssselect('h3.dataset-heading')[0].text_content()
print("The name of the most recently added dataset on data.gov:")
print(title.strip())

# 6. Write a Python program to extract h1 tag from example.com.
html = urlopen('http://www.example.com/')
bsh = BeautifulSoup(html.read(), 'html.parser')
print(bsh.h1)

# 7. Write a Python program to extract and display all the header tags from en.wikipedia.org/wiki/Main_Page. 
from urllib.request import urlopen
from bs4 import BeautifulSoup
html = urlopen('https://en.wikipedia.org/wiki/Main_Page')
bs = BeautifulSoup(html, "html.parser")
titles = bs.find_all(['h1', 'h2','h3','h4','h5','h6'])
print('List all the header tags :', *titles, sep='\n\n')

# 8. Write a Python program to extract and display all the image links from en.wikipedia.org/wiki/Peter_Jeffrey_(RAAF_officer). 
from urllib.request import urlopen
from bs4 import BeautifulSoup
import re

html = urlopen('https://en.wikipedia.org/wiki/Peter_Jeffrey_(RAAF_officer)')
bs = BeautifulSoup(html, 'html.parser')
images = bs.find_all('img', {'src':re.compile('.jpg')})
for image in images: 
    print(image['src']+'\n')

# 9. Write a Python program to get 90 days of visits broken down by browser for all sites on data.gov. 
r = requests.get("https://analytics.usa.gov/data/live/browsers.json")
print("90 days of visits broken down by browser for all sites:")
print(r.json()['totals']['browser'])

# 10. Write a Python program to that retrieves an arbitary Wikipedia page of "Python" and creates a list of links on that page. 
from urllib.request import urlopen
from bs4 import BeautifulSoup
html = urlopen("https://en.wikipedia.org/wiki/Python")
bsObj = BeautifulSoup(html)
for link in bsObj.findAll("a"):
  if 'href' in link.attrs:
    print(link.attrs['href'])

# 11. Write a Python program to check whether a page contains a title or not. 
from urllib.request import urlopen
from urllib.error import HTTPError
from bs4 import BeautifulSoup
def getTitle(url):
    try:
        html = urlopen(url)
    except HTTPError as e:
        return None
    try:
        bsObj = BeautifulSoup(html.read(), "lxml")
        title = bsObj.body.h1
    except AttributeError as e:
        return None
    return title

print(getTitle("https://www.w3resource.com/"))
print(getTitle("http://www.example.com/"))

# 12. Write a Python program to list all language names and number of related articles in the order they appear in wikipedia.org. 
html = urlopen('https://www.wikipedia.org/')
bs = BeautifulSoup(html, "html.parser")
nameList = bs.findAll('a', {'class': 'link-box'})
for name in nameList:
  print(name.get_text())


# 13. Write a Python program to get the number of people visiting a U.S. government website right now. 
# Source: https: // analytics.usa.gov/data/live/realtime.json
#https://bit.ly/2lVhlLX
# via: https://analytics.usa.gov/
url = 'https://analytics.usa.gov/data/live/realtime.json'
j = requests.get(url).json()
print("Number of people visiting a U.S. government website-")
print("Active Users Right Now:")
print(j['data'][0]['active_visitors'])


# 14. Write a Python program get the number of security alerts issued by US-CERT in the current year. 
# Source: https: // www.us-cert.gov/ncas/alerts
#https://bit.ly/2lVhlLX
url = 'https://www.us-cert.gov/ncas/alerts'
doc = html.fromstring(requests.get(url).text)
print("The number of security alerts issued by US-CERT in the current year:")
print(len(doc.cssselect('.item-list li')))

# 15. Write a Python program to get the number of Pinterest accounts maintained by U.S. State Department embassies and missions. 
# Source: https: // www.state.gov/r/pa/ode/socialmedia/
#https://bit.ly/2lVhlLX
url = 'http://www.state.gov/r/pa/ode/socialmedia/'
doc = html.fromstring(requests.get(url).text)
pinlinks = [a for a in doc.cssselect(
    'a') if 'pinterest.com' in str(a.attrib.get('href'))]
print("The number of Pinterest accounts maintained by U.S. State Department embassies and missions:")
print(len(pinlinks))

# 16. Write a Python program to get the number of followers of a given twitter account. 
handle = input('Input your account name on Twitter: ')
temp = requests.get(f'https://twitter.com/{handle}')
bs = BeautifulSoup(temp.text, 'lxml')
try:
    follow_box = bs.find(
        'li', {'class': 'ProfileNav-item ProfileNav-item--followers'})
    followers = follow_box.find('a').find(
        'span', {'class': 'ProfileNav-value'})
    print(f"Number of followers: {followers.get('data-count')} ")
except:
    print('Account name not found...')


# 17. Write a Python program to get the number of following on Twitter. 

handle = input('Input your account name on Twitter: ')
temp = requests.get(f'https://twitter.com/{handle}')
bs = BeautifulSoup(temp.text, 'lxml')

try:
    following_box = bs.find(
        'li', {'class': 'ProfileNav-item ProfileNav-item--following'})
    following = following_box.find('a').find(
        'span', {'class': 'ProfileNav-value'})
    print(f"{handle} is following {following.get('data-count')} people.")

except:
    print('Account name not found...')

# 18. Write a Python program to get the number of post on Twitter liked by a given account. 

handle = input('Input your account name on Twitter: ')
temp = requests.get(f'https://twitter.com/{handle}')
bs = BeautifulSoup(temp.text, 'lxml')

try:
    favorite_box = bs.find(
        'li', {'class': 'ProfileNav-item ProfileNav-item--favorites'})
    favorite = favorite_box.find('a').find(
        'span', {'class': 'ProfileNav-value'})
    print(f"Number of post {handle}  liked are {favorite.get('data-count')}: ")

except:
    print('Account name not found...')

# 19. Write a Python program to count number of tweets by a given Twitter account. 

handle = input('Input your account name on Twitter: ')
temp = requests.get(f'https://twitter.com/{handle}')
bs = BeautifulSoup(temp.text, 'lxml')

try:
    tweet_box = bs.find(
        'li', {'class': 'ProfileNav-item ProfileNav-item--tweets is-active'})
    tweets = tweet_box.find('a').find('span', {'class': 'ProfileNav-value'})
    print(f"{handle} tweets {tweets.get('data-count')} number of tweets.")

except:
    print('Account name not found...')

# 20. Write a Python program to scrap number of tweets of a given Twitter account. 
from bs4 import BeautifulSoup
import requests
handle = input('Input your account name on Twitter: ')
ctr = int(input('Input number of tweets to scrape: '))
res = requests.get(f'https://twitter.com/{handle}')
bs=BeautifulSoup(res.content,'lxml')
if all_tweets := bs.find_all('div', {'class': 'tweet'}):
    for tweet in all_tweets[:ctr]:
      context = tweet.find('div',{'class':'context'}).text.replace("\n"," ").strip()
      content = tweet.find('div',{'class':'content'})
      header = content.find('div',{'class':'stream-item-header'})
      user = header.find('a',{'class':'account-group js-account-group js-action-profile js-user-profile-link js-nav'}).text.replace("\n"," ").strip()
      time = header.find('a',{'class':'tweet-timestamp js-permalink js-nav js-tooltip'}).find('span').text.replace("\n"," ").strip()
      message = content.find('div',{'class':'js-tweet-text-container'}).text.replace("\n"," ").strip()
      footer = content.find('div',{'class':'stream-item-footer'})
      stat = footer.find('div',{'class':'ProfileTweet-actionCountList u-hiddenVisually'}).text.replace("\n"," ").strip()
      if context:
        print(context)
      print(user,time)
      print(message)
      print(stat)
      print()
else:
    print("List is empty/account name not found.")

# 21. Write a Python program to find the live weather report(temperature, wind speed, description and weather) of a given city. 
import requests
from pprint import pprint
def weather_data(query):
    res = requests.get(
        f'http://api.openweathermap.org/data/2.5/weather?{query}&APPID=****************************8&units=metric'
    );
    return res.json();
def print_weather(result,city):
    print(f"{city}'s temperature: {result['main']['temp']}°C ")
    print(f"Wind speed: {result['wind']['speed']} m/s")
    print(f"Description: {result['weather'][0]['description']}")
    print(f"Weather: {result['weather'][0]['main']}")
def main():
    city=input('Enter the city:')
    print()
    try:
        query = f'q={city}';
        w_data=weather_data(query);
        print_weather(w_data, city)
        print()
    except:
      print('City name not found...')
if __name__=='__main__':
	main()

# 22. Write a Python program to display the date, days, title, city, country of next 25 Hackevents. 
import requests
from bs4  import BeautifulSoup
res = requests.get('https://hackevents.co/hackathons')
bs = BeautifulSoup(res.text, 'lxml')
hacks_data = bs.find_all('div',{'class':'hackathon '})
for i,f in enumerate(hacks_data,1):
    hacks_month = f.find('div',{'class':'date'}).find('div',{'class':'date-month'}).text.strip()
    hacks_date = f.find('div',{'class':'date'}).find('div',{'class':'date-day-number'}).text.strip()
    hacks_days = f.find('div',{'class':'date'}).find('div',{'class':'date-week-days'}).text.strip()
    hacks_final_date = f"{hacks_date} {hacks_month}, {hacks_days} "
    hacks_name = f.find('div',{'class':'info'}).find('h2').text.strip()
    hacks_city = f.find('div',{'class':'info'}).find('p').find('span',{'class':'city'}).text.strip()
    hacks_country = f.find('div',{'class':'info'}).find('p').find('span',{'class':'country'}).text.strip()
    print(
        "{:<5}{:<15}: {:<90}: {}, {}\n ".format(
            f'{str(i)})',
            hacks_final_date,
            hacks_name.title(),
            hacks_city,
            hacks_country,
        )
    )
      

# 23. Write a Python program to download IMDB's Top 250 data(movie name, Initial release, director name and stars). 
#https://bit.ly/2NyxdAG
from bs4 import BeautifulSoup
import requests
import re

# Download IMDB's Top 250 data
url = 'http://www.imdb.com/chart/top'
response = requests.get(url)
soup = BeautifulSoup(response.text, 'lxml')

movies = soup.select('td.titleColumn')
links = [a.attrs.get('href') for a in soup.select('td.titleColumn a')]
crew = [a.attrs.get('title') for a in soup.select('td.titleColumn a')]
ratings = [b.attrs.get('data-value') for b in soup.select('td.posterColumn span[name=ir]')]
votes = [b.attrs.get('data-value') for b in soup.select('td.ratingColumn strong')]

imdb = []

# Store each item into dictionary (data), then put those into a list (imdb)
for index in range(len(movies)):
    # Seperate movie into: 'place', 'title', 'year'
    movie_string = movies[index].get_text()
    movie = (' '.join(movie_string.split()).replace('.', ''))
    movie_title = movie[len(str(index))+1:-7]
    year = re.search('\((.*?)\)', movie_string)[1]
    place = movie[:len(str(index))-(len(movie))]
    data = {"movie_title": movie_title,
            "year": year,
            "place": place,
            "star_cast": crew[index],
            "rating": ratings[index],
            "vote": votes[index],
            "link": links[index]}
    imdb.append(data)

for item in imdb:
    print(item['place'], '-', item['movie_title'], '('+item['year']+') -', 'Starring:', item['star_cast'])

# 24. Write a Python program to get movie name, year and a brief summary of the top 10 random movies. 
from bs4 import BeautifulSoup
import requests
import random
def get_imd_movies(url):
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')
    movies = soup.find_all("td", class_="titleColumn")
    random.shuffle(movies)
    return movies
def get_imd_summary(url):
    movie_page = requests.get(url)
    soup = BeautifulSoup(movie_page.text, 'html.parser')
    return soup.find("div", class_="summary_text").contents[0].strip()

def get_imd_movie_info(movie):
    movie_title = movie.a.contents[0]
    movie_year = movie.span.contents[0]
    movie_url = 'http://www.imdb.com' + movie.a['href']
    return movie_title, movie_year, movie_url

def imd_movie_picker():
    ctr=0
    print("--------------------------------------------")
    for movie in get_imd_movies('http://www.imdb.com/chart/top'):
        movie_title, movie_year, movie_url = get_imd_movie_info(movie)
        movie_summary = get_imd_summary(movie_url)
        print(movie_title, movie_year)
        print(movie_summary)
        print("--------------------------------------------")
        ctr=ctr+1
        if (ctr==10):
          break;   
if __name__ == '__main__':
    imd_movie_picker()
  
# 25. Write a Python program to get the number of magnitude 4.5 + earthquakes detected worldwide by the USGS.
#https://bit.ly/2lVhlLX
# landing page:
# http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
csvurl = 'http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv'
rows = list(csv.DictReader(requests.get(csvurl).text.splitlines()))
print("The number of magnitude 4.5+ earthquakes detected worldwide by the USGS:", len(rows))


# 26. Write a Python program to display the contains of different attributes like different attributes like status_code, headers, url, history, encoding, reason, cookies, elapsed, request and content of a specified resource. 
response = requests.get('https://python.org')
print("Status Code: ", response.status_code)
print("Headers: ", response.headers)
print("Url: ", response.url)
print("History: ", response.history)
print("Encoding: ", response.encoding)
print("Reason: ", response.reason)
print("Cookies: ", response.cookies)
print("Elapsed: ", response.elapsed)
print("Request: ", response.request)
print("Content: ", response._content)

# 27. Write a Python program to verifiy SSL certificates for HTTPS requests using requests module. 
# Note: Requests verifies SSL certificates for HTTPS requests, just like a web browser. By default, SSL verification is enabled, and Requests will throw a SSLError if it's unable to verify the certificate
import requests
print(requests.get('https://w3resource.com'))
print(requests.get('https://wayback.com'))
  
