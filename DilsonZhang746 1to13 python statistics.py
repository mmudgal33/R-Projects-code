# -*- coding: utf-8 -*-
"""
Created on Thu Mar 27 13:13:11 2025

@author: Radha Sharma
"""

# -*- coding: utf-8 -*-

# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

# https://www.w3resource.com/python-exercises/
# https://www.w3resource.com/sql-exercises/adventureworks/adventureworks-exercises.php


###String Format Manipulation in Python
#example
print("Please enter the amount of dollar")
money = eval(input("Dollars: "))
print("The total value of your change is ${0:0.2f}".format(money))

#The simplest template strings just specify where to 
#plug in the parameters.
#'Hello Mr. Smith, you may have won $10000'
print("Hello {0} {1}, you may have won ${2}".format("Mr.","Smith", 10000))



#control the width and/ or precision of a numeric value
#'This int,     7, was placed in a field of width 5'
print("This int, {0:5}, was placed in a field of width 5".format(7))


#'This float,       3.1416, has width 10 and precision 5'
print("This float, {0:10.5}, has width 10 and precision 5".format(3.1415926))


#'This float,       3.14159, is fixed at 5 decimal places'
print("This float, {0:10.5f}, is fixed at 5 decimal places".format(3.1415926))


#'This float, 3.1416, has width 0 and precision 5'
print("This float, {0:0.5}, has width 0 and precision 5".format(3.1415926))


#'Compare 3.14 and 3.1400000000000001243'
print("Compare {0} and {0:0.20}".format(3.14))




##Justification
#'left justification : Hi ! '
print(" leftjustification: {0:<10}".format("Hi !"))


#'right justification : Hi! '
print(" right justification:{0:>10}".format("Hi !"))


#'centered : Hi ! '
print("centered : {0:^5}".format("Hi!"))

















"""
Created on Sun Dec 11 22:14:40 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""
##Section 2 Variables and simple data types

##Lecture 1. String Variables
message = "Hello Python world!"
print(message)

#single and double quotes
message = 'I told my friend, "Python is my favorite language!"'
print(message)


#Changing Case in a String with Methods
name = "ada lovelace"
print(name.title())     #each word's first letter er capitalized

name = "Ada Lovelace"
print(name.upper())     #all letters are uppercase
print(name.lower())     #all letter are lowercase


#Combining or Concatenating Strings:
first_name = "ada"
last_name = "lovelace"
full_name = first_name + " " + last_name
print(full_name)

message = "Hello, " + full_name.title() + "!"
print(message)

#Adding Whitespace to Strings with Tabs or Newlines
print("Python")
print("\tPython")    #add tab space

print("Languages:\nPython\nC\nJavaScript")   #new line
print("Languages:\n\tPython\n\tC\n\tJavaScript")  #newline and tab


#Stripping Whitespace
favorite_language = 'python '
favorite_language

favorite_language.rstrip()   #rightside whitespace removed temporarily
favorite_language

#To remove the whitespace from the string permanently, you have to
#store the stripped value back into the variable
favorite_language = 'python '
favorite_language = favorite_language.rstrip()
favorite_language


favorite_language = ' python '
favorite_language.lstrip()    ##remove whitespace at left side
favorite_language.strip()     #remove whitespace at both side



##lecture 2. Numbers
3 ** 2      #exponents
10 % 3      #modulo

#Avoiding Type Errors with the str() Function
age = 23
message = "Happy " + str(age) + "rd Birthday!"

print(message)


abs(-8-6) # |-14| = 14

min(-8,-6)
max(-8,-6)


# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 3 Working with Lists

##Lecture 1. Introducing lists
Numberlist = [1,2,3,4,5] 


bicycles = ['trek', 'cannondale', 'redline', 'specialized']
print(bicycles)

#Accessing Elements in a List
print(bicycles[0])
print(bicycles[0].title())  #combined with string method

#index start from 0
print(bicycles[1])    #second item
print(bicycles[3])    #fourth item
print(bicycles[-1])      #access the last item in a list
print(bicycles[-2]) #the second item from the end of the list

#Using Individual Values from a List
message = "My first bicycle was a " + bicycles[0].title() + "."

print(message)











##Lecture 2. Changing, Appending,Removing items of Lists
#Changing elements of a list
motorcycles = ['honda', 'yamaha', 'suzuki', 'ducati'] 
print(motorcycles)
dir(motorcycles)

motorcycles[0] = 'ducati'
print(motorcycles)

#append items into a list
motorcycles = ['honda', 'yamaha', 'suzuki']
print(motorcycles)

motorcycles.append('ducati')
print(motorcycles)

#append items into a list, starting with an empty list
motorcycles = []           #start with an empty list
motorcycles.append('honda')     
motorcycles.append('yamaha')
motorcycles.append('suzuki')
print(motorcycles)

#insert element into a list
motorcycles = ['honda', 'yamaha', 'suzuki']
motorcycles.insert(0, 'ducati')    
print(motorcycles)

#remove item from a list
#Removing an Item according to its position Using the del Statement
motorcycles = ['honda', 'yamaha', 'suzuki']
print(motorcycles)
del motorcycles[0]  #first item removed 
print(motorcycles)

motorcycles = ['honda', 'yamaha', 'suzuki']
print(motorcycles)
del motorcycles[1]      #second item removed  
print(motorcycles)


#Removing an Item Using the pop() Method
motorcycles = ['honda', 'yamaha', 'suzuki']
print(motorcycles)
popped_motorcycle = motorcycles.pop()
print(motorcycles)
print(popped_motorcycle)

motorcycles = ['honda', 'yamaha', 'suzuki']
last_owned = motorcycles.pop()
print("The last motorcycle I owned was a " + last_owned.title() + ".")

#Popping Items from any Position in a List
motorcycles = ['honda', 'yamaha', 'suzuki']
first_owned = motorcycles.pop(0)
print('The first motorcycle I owned was a ' + first_owned.title() + '.')

#Removing an Item by Value
motorcycles = ['honda', 'yamaha', 'suzuki', 'ducati']
print(motorcycles)
motorcycles.remove('ducati')
print(motorcycles)

motorcycles = ['honda', 'yamaha', 'suzuki', 'ducati']
print(motorcycles)
too_expensive = 'ducati'
motorcycles.remove(too_expensive)
print(motorcycles)
print("\nA " + too_expensive.title() + " is too expensive for me.")













##Lecture 3. Organizing lists
#Sorting a List Permanently with the sort() Method
cars = ['bmw', 'audi', 'toyota', 'subaru']
cars.sort()
print(cars)

cars = ['bmw', 'audi', 'toyota', 'subaru']
cars.sort(reverse=True)
print(cars)

#Sorting a List Temporarily with the sorted() Function
cars = ['bmw', 'audi', 'toyota', 'subaru']

print("Here is the original list:")
print(cars)

print("\nHere is the sorted list:")
print(sorted(cars))

print("\nHere is the reverse alphabetical list:")
print(sorted(cars, reverse=True))

print("\nHere is the original list again:")
print(cars)


#Printing a List in Reverse Order
cars = ['bmw', 'audi', 'toyota', 'subaru']
print(cars)
cars.reverse()
print(cars)


#Finding the Length of a List
cars = ['bmw', 'audi', 'toyota', 'subaru']
len(cars)










##Lecture 4. Looping Through a List
#Looping Through an Entire List
magicians = ['alice', 'david', 'carolina']
for magician in magicians:
    print(magician)        #need tab indent here
    

#Doing More Work Within a for Loop
magicians = ['alice', 'david', 'carolina'] 
for magician in magicians: 
    print(magician.title() + ", that was a great trick!")  
    print("I can't wait to see your next trick, " + magician.title() + ".\n")     
    
print("Thank you everyone, that was a great magic show!")








##lecture 5. Making Numerical Lists
#Using the range() Function
for value in range(1,5):
    print(value)

#Using range() to Make a List of Numbers
#using list() to make a list
numbers = list(range(1,6)) 
print(numbers) 


#list the even numbers between 1 and 10
even_numbers = list(range(2,11,2)) 
print(even_numbers)


#using append()
squares = []
for value in range(1,11):
    square = value**2
    squares.append(square)
    
print(squares)

#To write this code more concisely
squares = []
for value in range(1,11):
    squares.append(value**2)
print(squares)


num = [1,2,3]
# asked in infosys interview 16/04/2025
# fetch the iterator
iter_num = iter(num)

# asked question in infosys
print(num.next())  # error
print(iter_num.next())  # error
next(iter_num)
next(iter_num)
next(iter_num)
next(iter_num)  # error list ends









##Lecture 6. List comprehension and Working with Part of a List
#List comprehension
squares = [value**2 for value in range(1,11)]
print(squares)

#Working with Part of a List
#Slicing a List
players = ['charles', 'martina', 'michael', 'florence', 'eli']
print(players[0:3])         #return first three elements
print(players[1:4])         #return second ,third,fourth element
print(players[:4])      #automatically starts your slice at the beginning of the list
print(players[2:])      #slice that includes the end of a list
print(players[-3:])    #the last three players

players = ['charles', 'martina', 'michael', 'florence', 'eli'] 
print("Here are the first three players on my team:")
for player in players[:3]:
    print(player.title())
    
#copying a list
#if you copy a list, must use slice form
my_foods = ['pizza', 'falafel', 'carrot cake'] 
friend_foods = my_foods[:]     #useing slice form to copy a list

print("My favorite foods are:")
print(my_foods)

print("\nMy friend's favorite foods are:")
print(friend_foods)

#To prove that we actually have two separate lists
my_foods.append('cannoli') 
friend_foods.append('ice cream') 

print("My favorite foods are:")
print(my_foods)

print("\nMy friend's favorite foods are:")
print(friend_foods)


#using this form if to connect two list, then append item into one list
#will also do for the other
my_foods = ['pizza', 'falafel', 'carrot cake']
friend_foods = my_foods
my_foods.append('cannoli')
friend_foods.append('ice cream')
print("My favorite foods are:")
print(my_foods)
print("\nMy friend's favorite foods are:")
print(friend_foods)













##Lecture 7. tuples
dimensions = (200, 50)
print(dimensions[0])
print(dimensions[1])

#change one of the items in the tuple
dimensions = (200, 50)
dimensions[0] = 250     #will get error

#looping over all values in a tuple
dimensions = (200, 50)
print("Original dimensions:")
for dimension in dimensions:
    print(dimension)
    
#Writing over a Tuple   
dimensions = (400, 100)
print("\nModified dimensions:")
for dimension in dimensions:
    print(dimension)


dimensions = (200, 50, 400, 100)
dir(dimensions)
dimensions[0]
dimensions.count(400)









##Lecture 8 Using a while Loop with Lists
#Moving Items from One List to Another
# Start out with some users that need to be verified,
#  and an empty list to hold confirmed users.
unconfirmed_users = ['alice', 'brian', 'candace']
confirmed_users = []

# Verify each user, until there are no more unconfirmed users.
#  Move each verified user into the list of confirmed users.
while unconfirmed_users:
    current_user = unconfirmed_users.pop()
    
    print("Verifying user: " + current_user.title())
    confirmed_users.append(current_user)
    
# Display all confirmed users.
print("\nThe following users have been confirmed:")
for confirmed_user in confirmed_users:
    print(confirmed_user.title())
    
    
    
#Removing All Instances of Specific Values from a List
pets = ['dog', 'cat', 'dog', 'goldfish', 'cat', 'rabbit', 'cat']
print(pets)

while 'cat' in pets:
    pets.remove('cat')
    
print(pets)


















### Lecture 9. Set operation


## Create a set

empty_set = set()

empty_set
dir(empty_set)

even_numbers = {0, 2, 4, 6, 8}
even_numbers

odd_numbers = {1, 3, 5, 7, 9}
odd_numbers


##Convert from Other Data Types with set()

#from a string
set( 'letters' )

#from a list
set( ['Dasher', 'Dancer', 'Prancer', 'Mason-Dixon'] ) 

#from a tuple
set( ('Ummagumma', 'Echoes', 'Atom Heart Mother') )

#from a dictionary
set( {'apple': 'red', 'orange': 'orange', 'cherry': 'red'} )


a = {1, 2}
b = {2, 3}

a.update(b)
a
a.union(b)
a

a.clear()
c=a.copy()
c,a
c.add(4)
a.add(5)
c,a

#set intersection
a & b
a.intersection(b)


#set union
a | b
a.union(b)


drinks = {
    'martini': {'vodka', 'vermouth'},
    'black russian': {'vodka', 'kahlua'},
    'white russian': {'cream', 'kahlua', 'vodka'},
    'manhattan': {'rye', 'vermouth', 'bitters'},
    'screwdriver': {'orange juice', 'vodka'}
    }

bruss = drinks['black russian']
wruss = drinks['white russian']

#set difference
a - b
a.difference(b)

print("Languages:\n\tPython\n\tC\n\tJavaScript")

print("bruss {0} /n/n wruss {1}".format(bruss,wruss))
color_list = ["Red", "Green", "White", "Black"]
print(f"bruss {bruss}  wruss {wruss}")

print(bruss - wruss)

print(wruss - bruss)


#The exclusive or (items in one set or the other, but not both)
# uses ^ or symmetric_difference():
a ^ b

a.symmetric_difference(b)


## Test for Value by Using in

#making a dictionary, with value parts are sets

drinks = {
    'martini': {'vodka', 'vermouth'},
    'black russian': {'vodka', 'kahlua'},
    'white russian': {'cream', 'kahlua', 'vodka'},
    'manhattan': {'rye', 'vermouth', 'bitters'},
    'screwdriver': {'orange juice', 'vodka'}
    }

dir(drinks)
type(drinks)

#loop through dicionary value part
for name, contents in drinks.items():
    if 'vodka' in contents:
        print(name)


for name, contents in drinks.items():
    if 'vodka' in contents and not ('vermouth' in contents or
    'cream' in contents):
        print(name)


# check for combinations of set values
# & here is set operator intersection as applied on sets

for name, contents in drinks.items():
    if contents & {'vermouth', 'orange juice'}:
        print((name,contents))


#we wanted vodka but neither cream nor vermouth:
    
for name, contents in drinks.items():
    if 'vodka' in contents and not contents & {'vermouth', 'cream'}:
        print(name)











# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 4 Conditional tests and If statement

##Lecture 1. Conditional tests
#an example
cars = ['audi', 'bmw', 'subaru', 'toyota']

for car in cars:
    if car == 'bmw':
        print(car.upper())
    else:
        print(car.title())
        
#Conditional Tests  
#Checking for Equality
car = 'bmw'
car == 'bmw'

car = 'audi'
car == 'bmw'

#Ignoring Case When Checking for Equality
car = 'Audi'
car == 'audi'     #False

car = 'Audi'
car.lower() == 'audi'    #True

car = 'Audi'
car.lower() == 'audi'  #The lower() function doesn’t change the value that was originally stored in car
car

#Checking for Inequality
requested_topping = 'mushrooms'
if requested_topping != 'anchovies':
    print("Hold the anchovies!")
    
#Numerical Comparisons
age = 18
age == 18 	#True
answer = 17
if answer != 42:
    print("That is not the correct answer. Please try again!")

age = 19
age < 21	#True
age <= 21	#True
age > 21	#False
age >= 21	#False

#use and to check multiple conditions
age_0 = 22
age_1 = 18
age_0 >= 21 and age_1 >= 21      #False
age_1 = 22
age_0 >= 21 and age_1 >= 21      #True

#Using or to Check Multiple Conditions
age_0 = 22
age_1 = 18
age_0 >= 21 or age_1 >= 21      #True
age_0 = 18
age_0 >= 21 or age_1 >= 21      #False

#Checking Whether a Value Is in a List
requested_toppings = ['mushrooms', 'onions', 'pineapple']
'mushrooms' in requested_toppings     #True
'pepperoni' in requested_toppings     #False

#Checking Whether a Value Is Not in a List
banned_users = ['andrew', 'carolina', 'david']
user = 'marie'

if user not in banned_users:
    print(user.title() + ", you can post a response if you wish.")






#Lecture 2.If Statements
#simple If statement
age = 19
if age >= 18:
    print("You are old enough to vote!")
    print("Have you registered to vote yet?")


#if-else Statements
age = 17
if age >= 18:
    print("You are old enough to vote!")
    print("Have you registered to vote yet?")
else:
    print("Sorry, you are too young to vote.")
    print("Please register to vote as soon as you turn 18!")
    
#The if-elif-else Chain
age = 12
if age < 4:
    print("Your admission cost is $0.")
elif age < 18:
    print("Your admission cost is $5.")
else:
    print("Your admission cost is $10.")
    

#Omitting the else Block
age = 12
if age < 4:
    price = 0
elif age < 18:
    price = 5
elif age < 65:
    price = 10
elif age >= 65:
    price = 5

print("Your admission cost is $" + str(price) + ".")









##Lecture 3.Using if Statements with Lists

#Checking for Special Items
requested_toppings = ['mushrooms', 'green peppers', 'extra cheese']
for requested_topping in requested_toppings:
    print("Adding " + requested_topping + ".")
print("\nFinished making your pizza!")


#pizzeria runs out of green peppers
requested_toppings = ['mushrooms', 'green peppers', 'extra cheese']
for requested_topping in requested_toppings:
    if requested_topping == 'green peppers':
        print("Sorry, we are out of green peppers right now.")
    else:
        print("Adding " + requested_topping + ".")
print("\nFinished making your pizza!")


#Checking That a List Is Not Empty
requested_toppings = []
if requested_toppings:
    for requested_topping in requested_toppings:
        print("Adding " + requested_topping + ".")
    print("\nFinished making your pizza!")
else:
    print("Are you sure you want a plain pizza?")



#Using Multiple Lists
available_toppings = ['mushrooms', 'olives', 'green peppers',
                      'pepperoni', 'pineapple', 'extra cheese']

requested_toppings = ['mushrooms', 'french fries', 'extra cheese']

for requested_topping in requested_toppings:
    if requested_topping in available_toppings:
        print("Adding " + requested_topping + ".")
    else:
        print("Sorry, we don't have " + requested_topping + ".")
        
print("\nFinished making your pizza!")









# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 5. Dictionaries

#Lecture 1. Working with dictionaries
#A simple example of dictionary
alien_0 = {'color': 'green', 'points': 5}
print(alien_0['color'])
print(alien_0['points'])

alien_0 = {'color': 'green', 'points': 5}
new_points = alien_0['points']
print("You just earned " + str(new_points) + " points!")


#Adding New Key-Value Pairs
alien_0 = {'color': 'green', 'points': 5}
print(alien_0)
alien_0['x_position'] = 0
alien_0['y_position'] = 25
print(alien_0)

#Starting with an Empty Dictionary
alien_0 = {}
alien_0['color'] = 'green'
alien_0['points'] = 5
print(alien_0)


#Modifying Values in a Dictionary
alien_0 = {'color': 'green'}
print("The alien is " + alien_0['color'] + ".")
alien_0['color'] = 'yellow'
print("The alien is now " + alien_0['color'] + ".")

alien_0 = {'x_position': 0, 'y_position': 25, 'speed': 'medium'}
print("Original position: " + str(alien_0['x_position']))

# Move the alien to the right.
# Figure out how far to move the alien based on its speed.
if alien_0['speed'] == 'slow':
    x_increment = 1
elif alien_0['speed'] == 'medium':
    x_increment = 2
else:
    # This must be a fast alien.
    x_increment = 3

# The new position is the old position plus the increment.
alien_0['x_position'] = alien_0['x_position'] + x_increment

print("New position: " + str(alien_0['x_position']))

#Removing Key-Value Pairs
alien_0 = {'color': 'green', 'points': 5}
print(alien_0)
del alien_0['points']
print(alien_0)













#Lecture 2. Looping Through a Dictionary
#Looping Through All Key-Value Pairs
user_0 = {'username': 'efermi',
          'first': 'enrico',
          'last': 'fermi',
          }

for key, value in user_0.items():
    print("\nKey: " + key)
    print("Value: " + value)


#A Dictionary of favorite programming languages for a number of people
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
for name, language in favorite_languages.items():
    print(name.title() + "'s favorite language is " +
        language.title() + ".")

#Looping Through All the Keys in a Dictionary
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
 
for name in favorite_languages.keys():
    print(name.title())


#access the value associated with any key you care about inside the loop
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
friends = ['phil', 'sarah']
for name in favorite_languages.keys():
    print(name.title())
    if name in friends:
        print(" Hi " + name.title() +
              ", I see your favorite language is " +
              favorite_languages[name].title() + "!")
        

#use the keys() method to find out if a particular person was polled
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
if 'erin' not in favorite_languages.keys():
    print("Erin, please take our poll!")
    

#Looping Through a Dictionary’s Keys in Order
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
for name in sorted(favorite_languages.keys()):
    print(name.title() + ", thank you for taking the poll.")

#Looping Through All Values in a Dictionary
favorite_languages = {
    'jen': 'python',
    'sarah': 'c',
    'edward': 'ruby',
    'phil': 'python',
    }
print("The following languages have been mentioned:")
for language in favorite_languages.values():
    print(language.title())















##Lecture 3. Nesting dictionaries
#A List of Dictionaries
alien_0 = {'color': 'green', 'points': 5}
alien_1 = {'color': 'yellow', 'points': 10}
alien_2 = {'color': 'red', 'points': 15}
aliens = [alien_0, alien_1, alien_2]
for alien in aliens:
    print(alien)
    
# Make an empty list for storing aliens.
aliens = []

# Make 30 green aliens.
for alien_number in range (0,30):
    new_alien = {'color': 'green', 'points': 5, 'speed': 'slow'}
    aliens.append(new_alien)

# Show the first 5 aliens:
for alien in aliens[:5]:
    print(alien)
    print("...")

# Show how many aliens have been created.
print("Total number of aliens: " + str(len(aliens)))

#to change the first three aliens to yellow, medium-speed aliens 
#worth 10 points each
for alien in aliens[0:3]:
    if alien['color'] == 'green':
        alien['color'] = 'yellow'
        alien['speed'] = 'medium'
        alien['points'] = 10
        

#A List in a Dictionary
# Store information about a pizza being ordered.
pizza = {
    'crust': 'thick',
    'toppings': ['mushrooms', 'extra cheese'],
    }

# Summarize the order.
print("You ordered a " + pizza['crust'] + "-crust pizza " +
      "with the following toppings:")

for topping in pizza['toppings']:
    print("\t" + topping)
    

#A List in a Dictionary
favorite_languages = {
        'jen': ['python', 'ruby'],
        'sarah': ['c'],
        'edward': ['ruby', 'go'],
        'phil': ['python', 'haskell'],
        }
for name, languages in favorite_languages.items():
    print("\n" + name.title() + "'s favorite languages are:")
    for language in languages:
        print("\t" + language.title())
        
        
#A Dictionary in a Dictionary
users = {'aeinstein': {'first': 'albert',
                       'last': 'einstein',
                       'location': 'princeton'},
         'mcurie': {'first': 'marie',
                    'last': 'curie',
                    'location': 'paris'},
         }

for username, user_info in users.items():
    print("\nUsername: " + username)
    full_name = user_info['first'] + " " + user_info['last']
    location = user_info['location']

    print("\tFull name: " + full_name.title())
    print("\tLocation: " + location.title())
        

        
    













### Lecture 4. get() method

d = {'Wilson': 32, 'Dudu': 20, 'Maomao': 22}
print(d.get('Wilson'))


#Python get() Method with default parameter.

d = {'Wilson': 32, 'Dudu': 20, 'Maomao': 22}
# since 4 is not in keys, it'll print "Not found"
print(d.get('Mia', "Not found"))



## using nested get()

test_dict = {'Wilson': {'Age': 32, 'Gender': 'male'},  'Dudu': {'Age': 20, 'Gender': 'male'}, 'Maomao': {'Age': 22, 'Gender': 'male'}}

res = test_dict.get('Wilson', {}).get('Age')

res


### Sort a list of dictionaries

dicts_lists = [
  {
    "Name": "Wilson",
    "Age": 32,
  },
  {
     "Name": "Dudu",
     "Age": 20,
  },
  {
    "Name": "Maomao",
    "Age": 22,
  }
]


#using dictionary's get method to sort by Age
dicts_lists.sort(key=lambda item: item.get("Age"))

dicts_lists




#Difference Between Python Dictionary get() method and dict[key]
# Using get() to get the value as a None
course={'language': 'python', 'fee': 4000}
print('duration:', course.get('duration'))



# Use dict[key], key is not in the dictionary
course={'language': 'python', 'fee': 4000}
print('duration:',course['duration'])
















##Lecture 4. Using a while Loop with Dictionaries
#Filling a Dictionary with User Input
responses = {}

# Set a flag to indicate that polling is active.
polling_active = True

while polling_active:
    # Prompt for the person's name and response.
    name = input("\nWhat is your name? ")
    response = input("Which mountain would you like to climb someday? ")
    
    # Store the response in the dictionary:
    responses[name] = response
    
    # Find out if anyone else is going to take the poll.
    repeat = input("Would you like to let another person respond? (yes/ no) ")
    if repeat == 'no':
        polling_active = False
        
# Polling is complete. Show the results.
print("\n--- Poll Results ---")
for name, response in responses.items():
    print(name + " would like to climb " + response + ".")
    


alien={}
alien['color']='green'
alien['points']=5
alien
alien.keys()
keys=list(alien.keys())
keys
keys=[key for key in alien]

data = {'key1': 100, 'key2': 200, 'key3': 300}
if 'key2' in data:
    value = data.pop('key2')
    print("Removed key2 with value:", value)
else:
    print("Key not found!")
    

man = {'name': 'Zubi', 'age': 23, 'salary': 850000.0}
result = man.popitem()
print('Return Value = ', result)
print('man = ', man)

# setdefault update if key:value pair not present
Dictionary1 = { 'A': 'Geeks', 'B': 'For' }
ret_value = Dictionary1.setdefault('C', "Geeks")
print("Return value of setdefault():", ret_value)
print("Dictionary after using setdefault():", Dictionary1)

# setdefault does not update if key:value pair already present
my_dict = {'a': 1, 'b': 2}
existing_value = my_dict.setdefault('a', 99)
print(my_dict)
print("Existing value: ", existing_value)

# update 
marks = {'Physics': 67, 'Maths': 87}
internal_marks = {'Chemistry': 48}
marks.update(internal_marks)
print(marks)  # Output: {'Physics': 67, 'Maths': 87, 'Practical': 48}
marks.update({'Chemistry': 49})
print(marks)  # Output: {'Physics': 67, 'Maths': 87, 'Practical': 49}

del marks['Practical']
marks.values()
marks.keys()

string = 'mohit mudgal'
dir(string)











# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 6. User Input and While Loop

##Lecture 1. User Input -part A
#How the input() Function Works
message = input("Tell me something, and I will repeat it back to you: ")
print(message)


#build a multi-line string for the prompt
prompt = "If you tell us who you are, we can personalize the messages you see."
prompt += "\nWhat is your first name? :  "


name = input(prompt)
print("\nHello, " + name + "!")



##Lecture 2. User Input -part B
#Using int() to Accept Numerical Input
height = input("How tall are you, in inches? ")
height = int(height)

if height >= 36:
    print("\nYou're tall enough to ride!")
else:
    print("\nYou'll be able to ride when you're a little older.")


#The Modulo Operator
number = input("Enter a number, and I'll tell you if it's even or odd: ")
number = int(number)   #Using int() to Accept Numerical Input

if number % 2 == 0:
    print("\nThe number " + str(number) + " is even.")
else:
    print("\nThe number " + str(number) + " is odd.")



##Lecture 3. Introducing while Loops-A
#loop counts from 1 to 5:
current_number = 1
while current_number <= 5:
    print(current_number)
    current_number += 1


#Letting the User Choose When to Quit
prompt = "\nTell me something, and I will repeat it back to you:"
prompt += "\nEnter 'quit' to end the program. "

message = ""
while message != 'quit':
    message = input(prompt)
    print(message)

#Does not show message when user input 'quit'
prompt = "\nTell me something, and I will repeat it back to you:"
prompt += "\nEnter 'quit' to end the program. "

message = ""
while message != 'quit':
    message = input(prompt)
    if message != 'quit':    #if input is quit, does not show this before quit
        print(message)



#Using a Flag
prompt = "\nTell me something, and I will repeat it back to you:"
prompt += "\nEnter 'quit' to end the program. "
  
active = True
while active:
    message = input(prompt)
    
    if message == 'quit':
        active = False
    else:
        print(message)





#Lecture 4.Using break and continue in while loops
#Using break to Exit a Loop
prompt = "\nPlease tell me a city you have visited:"
prompt += "\n(Enter 'quit' when you are finished.) "

while True:
    city = input(prompt)
    
    if city == 'quit':
        break
    else:
        print("I'd love to go to " + city.title() + "!")
        


#Using continue in a Loop
current_number = 0
while current_number < 10:
    current_number += 1
    if current_number % 2 == 0:
        continue
    print(current_number)




##Lecture 5 Using a while Loop with Lists
#Moving Items from One List to Another
# Start out with some users that need to be verified,
#  and an empty list to hold confirmed users.
unconfirmed_users = ['alice', 'brian', 'candace']
confirmed_users = []

# Verify each user, until there are no more unconfirmed users.
#  Move each verified user into the list of confirmed users.
while unconfirmed_users:
    current_user = unconfirmed_users.pop()
    
    print("Verifying user: " + current_user.title())
    confirmed_users.append(current_user)
    
# Display all confirmed users.
print("\nThe following users have been confirmed:")
for confirmed_user in confirmed_users:
    print(confirmed_user.title())
    
    
    
#Removing All Instances of Specific Values from a List
pets = ['dog', 'cat', 'dog', 'goldfish', 'cat', 'rabbit', 'cat']
print(pets)

while 'cat' in pets:
    pets.remove('cat')
    
print(pets)









##Lecture 6. Using a while Loop with Dictionaries
#Filling a Dictionary with User Input
responses = {}

# Set a flag to indicate that polling is active.
polling_active = True

while polling_active:
    # Prompt for the person's name and response.
    name = input("\nWhat is your name? ")
    response = input("Which mountain would you like to climb someday? ")
    
    # Store the response in the dictionary:
    responses[name] = response
    
    # Find out if anyone else is going to take the poll.
    repeat = input("Would you like to let another person respond? (yes/ no) ")
    if repeat == 'no':
        polling_active = False
        
# Polling is complete. Show the results.
print("\n--- Poll Results ---")
for name, response in responses.items():
    print(name + " would like to climb " + response + ".")
    











# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@python32
"""

##Section 7. Functions

##Lecture 1. Defining a function
#Defining a simple Function
def greet_user():
    """Display a simple greeting."""
    print("Hello!")

greet_user()


#Passing Information to a Function
def greet_user(username):
    """Display a simple greeting."""
    print("Hello, " + username.title() + "!")
    
greet_user('jesse')

greet_user('Wilson')














##Lecture 2.  Passing arguments to function
#Example of pets type and its name
def describe_pet(animal_type, pet_name):
    """Display information about a pet."""
    print("\nI have a " + animal_type + ".")
    print("My " + animal_type + "'s name is " + pet_name.title() + ".")

#Positional Arguments
describe_pet('hamster', 'harry')
describe_pet('dog', 'willie')

#Keyword Arguments
describe_pet(animal_type='hamster', pet_name='harry')
describe_pet(pet_name='harry', animal_type='hamster')


#Default Values
def describe_pet(pet_name, animal_type='dog'):
    """Display information about a pet."""
    print("\nI have a " + animal_type + ".")
    print("My " + animal_type + "'s name is " + pet_name.title() + ".")
    
# A dog named Willie.
describe_pet('willie')
describe_pet(pet_name='willie')


# Equivalent calls
# A dog named Willie.
describe_pet('willie')
describe_pet(pet_name='willie')

#A hamster named Harry
describe_pet('harry', 'hamster')
describe_pet(pet_name='harry', animal_type='hamster')
describe_pet(animal_type='hamster', pet_name='harry')












##Lecture 3. Functions: Returning a Simple Value
#a function that takes a first and last name, and returns 
#a neatly formatted full name
def get_formatted_name(first_name, last_name):
    """Return a full name, neatly formatted."""
    full_name = first_name + ' ' + last_name
    return full_name.title()
    
musician = get_formatted_name('jimi', 'hendrix')
print(musician)


#Making an Argument Optional
#A first attempt to include middle names might look like this:
def get_formatted_name(first_name, middle_name, last_name):
    """Return a full name, neatly formatted."""
    full_name = first_name + ' ' +  middle_name + ' ' + last_name
    return full_name.title()
    
musician = get_formatted_name('john', 'hooker', 'lee')
print(musician)



#A better solution is to to make the middle name optional. 
#We can give the middle_name argument an empty default value 
#and ignore the argument unless the user provides a value:
def get_formatted_name(first_name, last_name, middle_name=''):
    """Return a full name, neatly formatted."""
    if middle_name:
        full_name = first_name + ' ' +  middle_name + ' ' + last_name
    else:
        full_name = first_name + ' ' + last_name
    return full_name.title()
    
musician = get_formatted_name('jimi', 'hendrix')
print(musician)

musician = get_formatted_name('john', 'hooker', 'lee')
print(musician)















##Lecture 4. Functions: Return a Dictionary
#returns a dictionary representing a person
def build_person(first_name, last_name):
    """Return a dictionary of information about a person."""
    person = {'first': first_name, 'last': last_name}
    return person

musician = build_person('jimi', 'hendrix')
print(musician)



#Returning a Dictionary,'age' as optional
def build_person(first_name, last_name, age=''):
    """Return a dictionary of information about a person."""
    person = {'first': first_name, 'last': last_name}
    if age:
        person['age'] = age
    return person

musician = build_person('jimi', 'hendrix', age=27)
print(musician)















##Lecture 5. Using a Function with a while Loop
def get_formatted_name(first_name, last_name):
    """Return a full name, neatly formatted."""
    full_name = first_name + ' ' + last_name
    return full_name.title()

while True:
    print("\nPlease tell me your name:")
    print("(enter 'q' at any time to quit)")
    
    f_name = input("First name: ")
    if f_name == 'q':
        break
    l_name = input("Last name: ")
    if l_name == 'q':
        break
    formatted_name = get_formatted_name(f_name, l_name)
    print("\nHello, " + formatted_name + "!")
    













##Lecture 6. Passing a List to function
#Passing a List of names to a function called greet_users(),
#which greets each person in the list individually
def greet_users(names):
    """Print a simple greeting to each user in the list."""
    for name in names:
        msg = "Hello, " + name.title() + "!"
        print(msg)

usernames = ['hannah', 'ty', 'margot']
greet_users(usernames)



#Modifying a List in a Function
#does this without using functions
# Start with some designs that need to be printed.
unprinted_designs = ['iphone case', 'robot pendant', 'dodecahedron']
completed_models = []
# Simulate printing each design, until none are left.
# Move each design to completed_models after printing.
while unprinted_designs:
    current_design = unprinted_designs.pop()
    # Simulate creating a 3D print from the design.
    print("Printing model: " + current_design)
    completed_models.append(current_design)

# Display all completed models.
print("\nThe following models have been printed:")
for completed_model in completed_models:
    print(completed_model)
    

#Modifying a List in a Function, using 2 functions    
def print_models(unprinted_designs, completed_models):
    """
    Simulate printing each design, until there are none left.
    Move each design to completed_models after printing.
    """
    while unprinted_designs:
        current_design = unprinted_designs.pop()
    
        # Simulate creating a 3d print from the design.
        print("Printing model: " + current_design)
        completed_models.append(current_design)
        
def show_completed_models(completed_models):
    """Show all the models that were printed."""
    print("\nThe following models have been printed:")
    for completed_model in completed_models:
        print(completed_model)
        
        
unprinted_designs = ['iphone case', 'robot pendant', 'dodecahedron']
completed_models = []

print_models(unprinted_designs, completed_models)
show_completed_models(completed_models)
unprinted_designs  #empty now


#If you do not want to modify the original list
#You can use a copy of a list when you are calling the function
unprinted_designs = ['iphone case', 'robot pendant', 'dodecahedron']
completed_models = []

print_models(unprinted_designs[:], completed_models)
show_completed_models(completed_models)
unprinted_designs      #not modified











#Lecture 7. Passing an arbitrary number of arguments to function
#Passing an Arbitrary Number of Arguments to Function
def make_pizza(*toppings):
    """Print the list of toppings that have been requested."""
    print(toppings)
    
make_pizza('pepperoni')
make_pizza('mushrooms', 'green peppers', 'extra cheese')


#replace the print statement with a loop that runs through
#the list of toppings and describes the pizza being ordered
def make_pizza(*toppings):
    """Summarize the pizza we are about to make."""
    print("\nMaking a pizza with the following toppings:")
    for topping in toppings:
        print("- " + topping)
        
make_pizza('pepperoni')
make_pizza('mushrooms', 'green peppers', 'extra cheese')

# pass list directly, use str(topping)
def make_pizza(toppings):
    """Summarize the pizza we are about to make."""
    print("\nMaking a pizza with the following toppings:")
    # print(toppings)
    
    for topping in toppings:
        print("- " + str(topping))

toppings_list=['mushrooms', 'green peppers', 'extra cheese']
make_pizza(toppings_list)




#Mixing Positional and Arbitrary Arguments
def make_pizza(size, *toppings):
    """Summarize the pizza we are about to make."""
    print("\nMaking a " + str(size) +
          "-inch pizza with the following toppings:")
    for topping in toppings:
        print("- " + topping)
        
make_pizza(16, 'pepperoni')
make_pizza(12, 'mushrooms', 'green peppers', 'extra cheese')




#Using Arbitrary Keyword Arguments
def build_profile(first, last, **user_info):
    """Build a dictionary containing everything we know about a user."""
    profile = {}
    profile['first_name'] = first
    profile['last_name'] = last
    for key, value in user_info.items():
        profile[key] = value
    return profile

user_profile = build_profile('albert', 'einstein',
                             location='princeton',
                             field='physics')

print(user_profile)















##Lecture 8. Storing Your Functions in Modules
#Importing an Entire Module
#A module file pizza.py, saved in the same directory as this 
#source file in your computer.
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

#Now we can import the module pizza we just created and 
#then makes two calls to make_pizza():      
import pizza

# function inside pizza.py named make_pizza


pizza.make_pizza(16, 'pepperoni')
pizza.make_pizza(12, 'mushrooms', 'green peppers', 'extra cheese')






#Importing Specific Functions
from pizza import make_pizza
make_pizza(16, 'pepperoni')
make_pizza(12, "mushrooms", "green peppers", "extra cheese")


#Using as to Give a Function an Alias
from pizza import make_pizza as mp
mp(16, 'pepperoni')
mp(12, 'mushrooms', 'green peppers', 'extra cheese')


#Using as to Give a Module an Alias
import pizza as p
p.make_pizza(16, 'pepperoni')
p.make_pizza(12, 'mushrooms', 'green peppers', 'extra cheese')


#Importing All Functions in a Module
#*** just know this form if you see another code 
#best not use this form, however
from pizza import *
make_pizza(16, 'pepperoni')
make_pizza(12, 'mushrooms', 'green peppers', 'extra cheese')

















### Lecture 9. Map() function

# Return double of n
def summation(n):
    return n + n
 
# We double all numbers using map()
numbers = (1, 2, 3, 4)
result = map(summation, numbers)
print(list(result))


# Double all numbers using map and lambda
 
numbers = (1, 2, 3, 4)
result = map(lambda x: x + x, numbers)
print(list(result))



# Add two lists using map and lambda
 
numbers1 = [1, 2, 3]
numbers2 = [4, 5, 6]
 
result = map(lambda x, y: x + y, numbers1, numbers2)
print(list(result))



# List of strings
lst = ['sat', 'bat', 'cat', 'mat']
 
# map() can listify the list of strings individually
test = list(map(list, lst))
print(test)




# Define a function that doubles even numbers and leaves odd 
#numbers as is
def double_even(num):
    if num % 2 == 0:
        return num * 2
    else:
        return num
 
# Create a list of numbers to apply the function to
numbers = [1, 2, 3, 4, 5]
 
# Use map to apply the function to each element in the list
result = list(map(double_even, numbers))
 
# Print the result
print(result)  # [1, 4, 3, 8, 5]

















### Lecture 10. Lambda function

# A simple regular user-defined function, and its lambda function
#alternative

def f_square(x):
    return x**2

f_square(10)


lambda x: x**2

(lambda x: x**2)(10)

result= lambda x: x**2

result(10)


#A lambda function which contains simple conditional test.
calc = lambda num: "Odd number" if num % 2 != 0 else "Even number"
 
print(calc(20))
print(calc(19))



# Invoking lambda return value to perform various string operations
concate = lambda s: ''.join([char for char in s if not char.isdigit()])
print("filter_nums():", concate("Wilson32"))
 
do_exclaim = lambda s: s + '!'
print("do_exclaim():", do_exclaim("I like programming"))
 
find_sum = lambda n: sum([int(x) for x in str(n)])
print("find_sum():", find_sum(301301))
















### Lecture 11. Lambda and Map, Filter

#The lambda function gets more helpful when used inside a function.
#We can use lambda function inside map(), filter(), sorted() 
#and many other functions.

lst = ["23", "32", "56", "0", "-3", "-18"]
# sort list[str] numerically using sorted()
# and custom sorting key using lambda
print("Sorted numerically:",
      sorted(lst, key=lambda x: int(x)))
 
# filter positive even numbers
# using filter() and lambda function
print("Positive even numbers have been excluded:",
      list(filter(lambda x: not (int(x) % 2 == 0 and int(x) > 0), lst)))
 
# added 10 to each item after type and
# casting to int, then convert items to string again
print("Operation on each item using lambda and map()",
      list(map(lambda x: str(int(x) + 10), lst)))


# map(lambda x: str(int(x) + 10), lst)
# map(function, anylist)




















### Lecture 12. partial() function
from functools import partial

#example 1
  
# A normal function
def f(x1, x2, x3):
    return x1*2 + x2**2  + x3/2
  
# A partial function that calls f with
# x1 as 32, x2 as 22.
g = partial(f, 32,22)
  
# Calling g()
#Since we have pre-filled our function with some constant values
# of x1, x2. And g() just takes a single argument i.e. the variable x3.
print(g(20))



#Example 2

# A normal function
def add(x1, x2, x3):
    return 32 * x1 + 22 * x2 + x3
  
# A partial function with b = 1 and c = 2
add_part = partial(add, x3 = 20, x2 = 22)
  
# Calling partial function
#Since we have pre-filled our function with some constant values
# of x3, x2. And add_part() just takes a single argument i.e. 
#the variable x1.
print(add_part(32))














# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 8. Classes
##Lecture 1. Creating and Using a Class
#Creating a Dog Class
class Dog():
    """A simple attempt to model a dog."""
    
    def __init__(self, name, age):
        """Initialize name and age attributes."""
        self.name = name
        self.age = age
        
    def sit(self):
        """Simulate a dog sitting in response to a command."""
        print(self.name.title() + " is now sitting.")

    def roll_over(self):
        """Simulate rolling over in response to a command."""
        print(self.name.title() + " rolled over!")
        

#Making an Instance from a Class
my_dog = Dog('willie', 6)
your_dog = Dog('lucy', 3)


#Accessing Attributes of instance
print("My dog's name is " + my_dog.name.title() + ".")
print("My dog is " + str(my_dog.age) + " years old.")
print("\nYour dog's name is " + your_dog.name.title() + ".")
print("Your dog is " + str(your_dog.age) + " years old.")


#Calling Methods
my_dog.sit()
my_dog.roll_over()


your_dog.sit()
your_dog.roll_over()







##Lecture 2. Working with Classes and Instances
#A class representing car
class Car():
    """A simple attempt to represent a car."""

    def __init__(self, make, model, year):
        """Initialize attributes to describe a car."""
        self.make = make
        self.model = model
        self.year = year
        self.odometer_reading = 0
        
    def get_descriptive_name(self):
        """Return a neatly formatted descriptive name."""
        long_name = str(self.year) + ' ' + self.make + ' ' + self.model
        return long_name.title()
    
    def read_odometer(self):
        """Print a statement showing the car's mileage."""
        print("This car has " + str(self.odometer_reading) + " miles on it.")
        
    def update_odometer(self, miles):
        """
        Set the odometer reading to the given value.
        Reject the change if it attempts to roll the odometer back.
        """
        if miles >= self.odometer_reading:
            self.odometer_reading = miles
        else:
            print("You can't roll back an odometer!")

    def increment_odometer(self, miles):
       """Add the given amount to the odometer reading."""
       self.odometer_reading += miles


my_new_car = Car('audi', 'a4', 2016)
print(my_new_car.get_descriptive_name())
my_new_car.read_odometer()


#Modifying an Attribute’s Value Directly
my_new_car.odometer_reading = 23500
my_new_car.read_odometer()





#Modifying an Attribute’s Value Through a Method
my_used_car = Car('subaru', 'outback', 2013)
print(my_used_car.get_descriptive_name())
my_used_car.update_odometer(23500)
my_used_car.read_odometer()

my_used_car.update_odometer(20000)   #get message





#Incrementing an Attribute’s Value Through a Method
my_used_car = Car('subaru', 'outback', 2013)
print(my_used_car.get_descriptive_name())
my_used_car.read_odometer()
my_used_car.update_odometer(23500)
my_used_car.read_odometer()
my_used_car.increment_odometer(100)
my_used_car.read_odometer()







# ElectricCar(Car) has inherit car

##Lecture 3. Inheritance of Classes
#making a simple version of the ElectricCar class, which
#inherits from parent class Car.

class ElectricCar(Car):
    """Models aspects of a car, specific to electric vehicles."""

    def __init__(self,make, model, year):
        """
        Initialize attributes of the parent class.
        Then initialize attributes specific to an electric car.
        """
        super().__init__(make, model, year)
        #self.battery = Battery()

my_tesla = ElectricCar('tesla', 'model s', 2016)
print(my_tesla.get_descriptive_name())








##Lecture 4.Working with Attributes and Methods for the Child Class
#dd a battery attribute that’s specific to electric cars 
#and a method to report on this attribute in ElectricCar class
class ElectricCar(Car):
    """Models aspects of a car, specific to electric vehicles."""

    def __init__(self, make, model, year):
        """
        Initialize attributes of the parent class.
        Then initialize attributes specific to an electric car.
        """
        super().__init__(make, model, year)
        self.battery_size = 70
    
    def describe_battery(self):
        """Print a statement describing the battery size."""
        print("This car has a " + str(self.battery_size) + "-kWh battery.")
        

my_tesla = ElectricCar('tesla', 'model s', 2016)
print(my_tesla.get_descriptive_name())
my_tesla.describe_battery()





#Instances as Attributes
#create a Battery class
class Battery():
    """A simple attempt to model a battery for an electric car."""

    def __init__(self, battery_size=60):
        """Initialize the batteery's attributes."""
        self.battery_size = battery_size

    def describe_battery(self):
        """Print a statement describing the battery size."""
        print("This car has a " + str(self.battery_size) + "-kWh battery.")  
        
    def get_range(self):
        """Print a statement about the range this battery provides."""
        if self.battery_size == 60:
            range = 140
        elif self.battery_size == 85:
            range = 185
            
        message = "This car can go approximately " + str(range)
        message += " miles on a full charge."
        print(message)
    

class ElectricCar(Car):
    """Models aspects of a car, specific to electric vehicles."""

    def __init__(self, make, model, year):
        """
        Initialize attributes of the parent class.
        Then initialize attributes specific to an electric car.
        """
        super().__init__(make, model, year)
        self.battery = Battery()


my_tesla = ElectricCar('tesla', 'model s', 2016)
print(my_tesla.get_descriptive_name())
my_tesla.battery.describe_battery()
my_tesla.battery.get_range()







##Lecture 5. Import Classes
#Importing a Single Class
#module file car.py stores Car class
from car import Car

my_new_car = Car('audi', 'a4', 2015)
print(my_new_car.get_descriptive_name())

my_new_car.odometer_reading = 23
my_new_car.read_odometer()


#Storing Multiple Classes in a Module
#module file car.py stores multiple class: Car, ElectricCar
#Battery
from car import ElectricCar
my_tesla = ElectricCar('tesla', 'model s', 2016)
print(my_tesla.get_descriptive_name())
my_tesla.battery.describe_battery()
my_tesla.battery.get_range()




#Importing Multiple Classes from a Module
from car import Car, ElectricCar
my_beetle = Car('volkswagen', 'beetle', 2016)
print(my_beetle.get_descriptive_name())
my_tesla = ElectricCar('tesla', 'roadster', 2016)
print(my_tesla.get_descriptive_name())


#Importing an Entire Module
import car
my_beetle = car.Car('volkswagen', 'beetle', 2016)
print(my_beetle.get_descriptive_name())
my_tesla = car.ElectricCar('tesla', 'roadster', 2016)
print(my_tesla.get_descriptive_name())















# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""

##Section 9. Files and Exceptions
##Lecture 1. Reading from a File
#Reading an Entire File
#opens file pi_digits.txt, reads it, and prints the contents
#of the file to the screen:
with open('pi_digits.txt') as file_object:
     contents = file_object.read()
     print(contents.rstrip())
     
#use a relative file path
with open('test\\pi_digits.txt') as file_object:
    contents = file_object.read()
    print(contents.rstrip())
    
#use an absolute file path 
file_path = 'd:\\test\\pi_digits.txt'
with open(file_path) as file_object:
    contents = file_object.read()
    print(contents.rstrip())

    
#Reading Line by Line
#Reading Line by Line    
filename = 'pi_digits.txt'
with open(filename) as file_object:
    for line in file_object:
        print(line)

    
#remove extra blank lines
filename = 'pi_digits.txt'
with open(filename) as file_object:
    for line in file_object:
        print(line.rstrip()) 

    
#Making a List of Lines from a File
filename = 'pi_digits.txt'

with open(filename) as file_object:
    lines = file_object.readlines()

for line in lines:
    print(line.rstrip())  
    

#Working with a File’s Contents
filename = 'pi_digits.txt'
with open(filename) as file_object:
    lines = file_object.readlines()

pi_string = ''
for line in lines:
    pi_string += line.rstrip()

print(pi_string)
print(len(pi_string))



#get rid of whitespace at the left side of digits
filename = 'pi_digits.txt'
with open(filename) as file_object:
    lines = file_object.readlines()

pi_string = ''
for line in lines:
    pi_string += line.strip()    #use strip here 

print(pi_string)
print(len(pi_string))



#Large Files: One Million Digits
#print just the first 50 decimal places
filename = 'pi_million_digits.txt'

with open(filename) as file_object:
    lines = file_object.readlines()

pi_string = ''
for line in lines:
    pi_string += line.strip()

print(pi_string[:52] + "...")
print(len(pi_string))




#Is Your Birthday Contained in Pi?
filename = 'pi_million_digits.txt'

with open(filename) as file_object:
    lines = file_object.readlines()

pi_string = ''
for line in lines:
    pi_string += line.rstrip()

birthday = input("Enter your birthday, in the form mmddyy: ")
if birthday in pi_string:
    print("Your birthday appears in the first million digits of pi!")
else:
    print("Your birthday does not appear in the first million digits of pi.")


    
    



##Lecture 2. Writing to a File
#Writing to an Empty File
filename = 'programming.txt'

with open(filename, 'w') as file_object:
    file_object.write("I love programming")
    
    
#Writing Multiple Lines
filename = 'programming.txt'

with open(filename, 'w') as file_object:
    file_object.write("I love programming.\n")
    file_object.write("I love creating new games.\n")
    

#Appending to a File
filename = 'programming.txt'

with open(filename, 'a') as file_object:
    file_object.write("I also love finding meaning in large datasets.\n")
    file_object.write("I love creating apps that can run in a browser.\n")







##Lecture 3. Using try-except Blocks Exception
#Handling zero-division error
print(5/0)

try:
    print(5/0)
except ZeroDivisionError:
    print("You can't divide by zero!")

    
    
#Using Exceptions to Prevent Crashes
print("Give me two numbers, and I'll divide them.")
print("Enter 'q' to quit.")

#a loop never stopped unless with q
while True:
    first_number = input("\nFirst number: ")
    if first_number == 'q':
        break
    second_number = input("Second number: ")
    try:
        answer = int(first_number) / int(second_number)
    except ZeroDivisionError:
        print("You can't divide by 0!")
    else:
        print(answer)
        
        
        
        
        
        
        
        
        
        
##Lecture 4. Handling the FileNotFoundError Exception
#try to open and read a file which has not been saved in the
#current working directory
#FileNotFoundError
filename = 'alice1.txt'
with open(filename) as f_obj:
    contents = f_obj.read()
 
    
#Use a try-except block instead
filename = 'alice1.txt'

try:
    with open(filename) as f_obj:
        contents = f_obj.read()
except FileNotFoundError:
    msg = "Sorry, the file " + filename + " does not exist."
    print(msg)
    

#analyze text with try-except-else block    
filename = 'alice.txt'

try:
    with open(filename) as f_obj:
        contents = f_obj.read()
except FileNotFoundError:
    msg = "Sorry, the file " + filename + " does not exist."
    print(msg)
else:
    # Count the approximate number of words in the file.
    words = contents.split()
    num_words = len(words)
    print("The file " + filename + " has about " + str(num_words) + " words.")
    


#Create a function holding try-except-else block
#working with multiple books
def count_words(filename):
    """Count the approximate number of words in a file."""
    try:
        with open(filename, encoding='utf-8') as f_obj:
            contents = f_obj.read() 
    except FileNotFoundError:
        msg = "Sorry, the file " + filename + " does not exist."
        print(msg)
    else:
        # Count approximate number of words in the file.
        words = contents.split()
        num_words = len(words)
        print("The file " + filename + " has about " + str(num_words) + " words.")

filenames = ['alice.txt', 'siddhartha1.txt', 'moby_dick.txt', 'little_women.txt']
for filename in filenames:
    count_words(filename)


    

    
    


##Lecture 5. Using Try, Except, else and Finally in Python
#Try, Except, else and Finally
def divide(x, y):
    try:
        # Floor Division : Gives only Fractional
        # Part as Answer
        result = x // y
    except ZeroDivisionError:
        print("Sorry ! You are dividing by zero ")
    else:
        print("Yeah ! Your answer is :", result)
    finally: 
        # this block is always executed  
        # regardless of exception generation. 
        print('This is always executed')  
        
# Look at parameters and note the working of Program
divide(3, 2)
divide(3, 0)      



#Failing Silently with pass
#Failing Silently and do nothing with error
def count_words(filename):
    """Count the approximate number of words in a file."""
    try:
        with open(filename) as f_obj:
            contents = f_obj.read() 
    except FileNotFoundError:
        pass
    else:
        # Count approximate number of words in the file.
        words = contents.split()
        num_words = len(words)
        print("The file " + filename + " has about " + str(num_words) + " words.")

filenames = ['alice.txt', 'siddhartha1.txt', 'moby_dick.txt', 'little_women.txt']
for filename in filenames:
    count_words(filename)  

    

    
    


##Lecture 6. Storing Data using json() module
#stores a set of numbers using json()

import json

numbers = [2, 3, 5, 7, 11, 13]

filename = 'numbers.json'
with open(filename, 'w') as file_object:
    json.dump(numbers, file_object)


#reads these numbers back into memory
import json

filename = 'numbers.json'
with open(filename) as file_object:
    numbers = json.load(file_object)
    
print(numbers)





#Saving and Reading User-Generated Data
#storing the user’s name
import json

username = input("What is your name? ")
filename = 'username.json'
with open(filename, 'w') as f_obj:
    json.dump(username, f_obj)
    print("We'll remember you when you come back, " + username + "!")



#greets a user whose name has already been stored:
import json

filename = 'username.json'

with open(filename) as f_obj:
    username = json.load(f_obj)
    print("Welcome back, " + username + "!")







##Lecture 7. Refactoring
#a function includes storing, getting and greeting user. 
import json

def greet_user():
    """Greet the user by name."""
    filename = 'username.json'
    try:
        with open(filename) as f_obj:
            username = json.load(f_obj)
    except FileNotFoundError:
        username = input("What is your name? ")
        with open(filename, 'w') as f_obj:
            json.dump(username, f_obj)
            print("We'll remember you when you come back, " + username + "!")
    else:
        print("Welcome back, " + username + "!")


greet_user()





#moving the code for retrieving a stored username to a 
#separate function get_stored_username(), and moving
#the part of get new user to another function get_new_username():
    
import json

def get_stored_username():
    """Get stored username if available."""
    filename = 'username.json'
    try:
        with open(filename) as f_obj:
            username = json.load(f_obj)
    except FileNotFoundError:
        return None
    else:
        return username

def get_new_username():
    """Prompt for a new username."""
    username = input("What is your name? ")
    filename = 'username.json'
    with open(filename, 'w') as f_obj:
        json.dump(username, f_obj)
    return username

def greet_user():
    """Greet the user by name."""
    username = get_stored_username()
    if username:
        print("Welcome back, " + username + "!")
    else:
        username = get_new_username()
        print("We'll remember you when you come back, " + username + "!")

greet_user()

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 # -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""


### Section 11. Introducing The Pandas library
##Lecture 1. Getting started with pandas

import pandas as pd
import numpy as np



# ### Declaring a Series 

s = pd.Series([12, -4, 7, 9])
s


s = pd.Series([12, -4, 7, 9], index=['a', 'b', 'c', 'd'])
s


s.values


s.index


# Defining a DataFrame

data = {'color' : ['blue', 'green', 'yellow', 'red', 'white'],
        'object' : ['ball', 'pen', 'pencil', 'paper', 'mug'],
        'price' : [1.2, 1.0, 0.6, 0.9, 1.7]}
frame = pd.DataFrame(data)
frame


















##Lecture 2. Introduction of pandas Data Structures: The Series
import pandas as pd
import numpy as np

# ### Declaring a Series 

s = pd.Series([12, -4, 7, 9])
s


s = pd.Series([12, -4, 7, 9], index=['a', 'b', 'c', 'd'])
s


s.values


s.index


# ### Selecting the Internal Elements

s[2]


s['b']


s[0:2]


s[['b','c']]


# ### Assigning Values to the Elements

s[1] = 0
s


s['b'] = 1
s


##Series object itself and its index have a name attribute
s.name = 'count'
s.index.name = 'alphabet'
s


# ### Defining Series from NumPy Arrays and Other Series

arr = np.array([1, 2, 3, 4])
s3 = pd.Series(arr)
s3


s4 = pd.Series(s)
s4






















### Lecture 3. Pandas Series Operations

# ### Filtering Values 
import pandas as pd
import numpy as np
s = pd.Series([12, -4, 7, 9], index=['a', 'b', 'c', 'd'])
s

s[s > 8]


# ### Mathematical Operations

s / 2

np.log(s)

np.exp(s)

# ### Evaluating Values

serd = pd.Series([1,0,2,1,2,3], index=['white', 'white', 'blue', 'green', 'green', 'yellow'])
serd


serd.unique()


serd.value_counts()


serd.isin([0,3])


serd[serd.isin([0,3])]


# ### NaN Values

s2 = pd.Series([5, -3, np.NaN, 14])
s2


s2.isnull()

s2[s2.isnull()]


s2.notnull()


s2[s2.notnull()]



# ### Series as Dictionaries


mydict = {'red': 2000, 'blue': 1000, 'yellow': 500, 'orange': 1000}
myseries = pd.Series(mydict)
myseries



colors = ['red', 'yellow', 'orange', 'blue', 'green']
myseries = pd.Series(mydict, index=colors)
myseries


# ### Operations between Series

mydict2 = {'red': 400, 'yellow': 1000, 'black': 700}
myseries2 = pd.Series(mydict2)
myseries + myseries2




















###Lecture 4. Introduction of pandas Data Structures: The DataFrame

# ### Defining a DataFrame
import pandas as pd
import numpy as np
data = {'color' : ['blue', 'green', 'yellow', 'red', 'white'],
        'object' : ['ball', 'pen', 'pencil', 'paper', 'mug'],
        'price' : [1.2, 1.0, 0.6, 0.9, 1.7]}
frame = pd.DataFrame(data)
frame


frame2 = pd.DataFrame(data, columns=['object', 'price'])
frame2



frame2 = pd.DataFrame(data, index=['one', 'two', 'three', 'four', 'five'])
frame2

frame3 = pd.DataFrame(np.arange(16).reshape((4,4)),
                     index=['red', 'blue', 'yellow', 'white'],
                     columns=['ball', 'pen', 'pencil', 'paper'])
frame3


# ### Selecting Elements


frame.columns


frame.index


frame.values

#selecting columns
frame['price']


frame.price

#to select elements using numeric or axis labels,using loc()
a = frame.loc[2]
a

frame.loc[[2,4]]

frame.loc[[0,3], ['color','price']]

#to select elements using integer indexing,using iloc()
frame.iloc[2]


frame.iloc[[2,4]]

frame.iloc[[0,3], [0,2]]


#to select rows, return a dataframe
frame[0:1]



frame[1:3]


# to select a single value

frame['object'][3]






























###Lecture 5. Basic manipulation of Pandas DataFrame


#Assigning label names to index and columns
import pandas as pd
import numpy as np
data = {'color' : ['blue', 'green', 'yellow', 'red', 'white'],
        'object' : ['ball', 'pen', 'pencil', 'paper', 'mug'],
        'price' : [1.2, 1.0, 0.6, 0.9, 1.7]}
frame = pd.DataFrame(data)

frame.index.name = 'id'; 
frame.columns.name = 'item'
frame


#add a new column
frame['new'] = 12
frame


frame['new'] = [3.0, 1.3, 2.2, 0.8, 1.1]
frame


ser = pd.Series(np.arange(5))
ser


frame['new'] = ser
frame


frame['price'][2] = 3.3



# ### Membership of a Value

frame.isin([1.0, 'pen'])


frame[frame.isin([1.0, 'pen'])]


# ### Deleting a Column

del frame['new']
frame


# ### Filtering 
data = {'price1' : [1.2, 1.0, 0.6, 0.9, 1.7],
        'pric2' : [2.1, 2.3, 0.9, 0.7, 1.9],
        'pric3' : [1.8, 3.0, 1.3, 1.6, 0.2]}
frame2 = pd.DataFrame(data)
frame2[frame2 < 1.2]


# ### DataFrame from Nested dict 


nestdict = {'red': { 2012: 22, 2013: 33},
            'white': { 2011: 13, 2012: 22, 2013: 16},
            'blue': { 2011: 17, 2012: 27, 2013: 18}}
frame2 = pd.DataFrame(nestdict)
frame2


# ### Transposition of a DataFrame 

frame3 = pd.DataFrame(np.arange(16).reshape((4,4)),
                     index=['red', 'blue', 'yellow', 'white'],
                     columns=['ball', 'pen', 'pencil', 'paper'])
frame3
frame3.T



























###Lecture 6. Working with Index of Pandas Data Structures
# ## The Index Objects

import pandas as pd
import numpy as np
ser = pd.Series([5, 0, 3, 8, 4], index=['red', 'blue', 'yellow', 'white', 'green'])
ser.index


# ### Methods on Index


ser.idxmin()


ser.idxmax()


# ### Index with Duplicate Labels

serd = pd.Series(range(6), index=['white', 'white', 'blue', 'green', 'green', 'yellow'])
serd


serd['white']


serd.index.is_unique


data = {'color' : ['blue', 'green', 'yellow', 'red', 'white'],
        'object' : ['ball', 'pen', 'pencil', 'paper', 'mug'],
        'price' : [1.2, 1.0, 0.6, 0.9, 1.7]}
frame = pd.DataFrame(data)

frame.index.is_unique


##reindexing

ser = pd.Series([2, 5, 7, 4], index=['one', 'two', 'three', 'four'])
ser


ser.reindex(['three', 'four', 'five', 'one'])



ser3 = pd.Series([1, 5, 6, 3], index=[0, 3, 5, 6])
ser3



ser3.reindex(range(6), method='ffill')


ser3.reindex(range(6), method='bfill')


frame.reindex(range(5), method='ffill', columns=['colors', 'price', 'new', 'object'])

frame.reindex(range(5), method='bfill', columns=['colors', 'price', 'new', 'object'])

# ### Dropping


ser = pd.Series(np.arange(4.), index=['red', 'blue', 'yellow', 'white'])
ser


ser.drop('yellow')


ser.drop(['blue','white'])



frame = pd.DataFrame(np.arange(16).reshape((4,4)), 
                    index=['red', 'blue', 'yellow', 'white'],
                    columns=['ball', 'pen', 'pencil', 'paper'])
frame


frame.drop(['blue','yellow'])


frame.drop(['pen','pencil'], axis=1)



























# ### Lecture 7. Operations, Functions, and Mapping of Pandas Data Structures
import pandas as pd
import numpy as np

#Arithmetic operations and Data Alignment
s1 = pd.Series([3,2,5,1],['white','yellow','green','blue'])
s2 = pd.Series([1,4,7,2,1],['white','yellow','black','blue','brown'])

s1 + s2


frame1 = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame2 = pd.DataFrame(np.arange(12).reshape((4,3)),
          index=['blue', 'green', 'white', 'yellow'],
          columns=['mug','pen','ball'])
frame1

frame2

frame1 + frame2


# ### Flexible Arithmetic Methods

frame1.add(frame2)


# ### Operations between DataFrame and Series

frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame


ser = pd.Series(np.arange(4), index=['ball','pen','pencil','paper'])
ser


frame - ser


ser['mug'] = 9
ser

frame - ser


# ## Function Application and Mapping

# ### Functions by Element

frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
np.sqrt(frame)


# ### Functions by Row or Column 

f = lambda x: x.max() - x.min()

def f(x):
    return x.max() - x.min()

frame.apply(f)



frame.apply(f, axis=1)



def f(x):
    return pd.Series([x.min(), x.max()], index=['min', 'max'])
frame.apply(f)























### Lecture 8. Statistics functions
import pandas as pd
import numpy as np

frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame.sum()         #sum of each column

frame.sum(axis=1)       #sum of each row

frame.mean()

frame.cumsum()

frame.cumsum(axis=1)

frame.describe()


# ## Correlation and Covariance

seq = pd.Series([1,2,3,4,4,3,2,1],['2006','2007','2008','2009','2010','2011','2012','2013'])
seq2 = pd.Series([3,4,3,4,5,4,3,2],['2006','2007','2008','2009','2010','2011','2012','2013'])
seq.corr(seq2)


seq.cov(seq2)


frame2 = pd.DataFrame([[1,4,3,6],[4,5,6,1],[3,3,1,5],[4,1,6,4]],
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame2


frame2.corr()


frame2.cov()


ser = pd.Series([0, 1, 2, 3, 9], index=['red','blue','yellow','white','green'])
ser


frame2.corrwith(ser)


frame2.corrwith(frame)























# ## Lecture 9. Sorting and Ranking of Pandas Data Structures
import pandas as pd
import numpy as np
ser = pd.Series([5, 0, 3, 8, 4], index=['red','blue','yellow','white','green'])
ser

ser.sort_index()


ser.sort_index(ascending=False)


frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame


frame.sort_index()


frame.sort_index(axis=1)



ser.sort_values()


frame.sort_values(by='pen')


frame.sort_values(by=['pen','pencil'])


ser.rank()          


ser.rank(ascending=False)


























# ## Lecture 10. Handling "Not a Number" Data with Pandas Data Structures

# ### Assigning a NaN Value
import pandas as pd
import numpy as np
ser = pd.Series([0,1,2,np.NaN,9], index=['red','blue','yellow','white','green'])
ser


ser['white'] = None
ser['white'] = np.NaN
ser



frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame

frame['ball']['red'] = np.NaN
frame['ball']['red'] = None

# ### FIltering Out NaN Values

ser.dropna()

ser[ser.notnull()]



frame3 = pd.DataFrame([[6,np.nan,6],[np.nan,np.nan,np.nan],[2,np.nan,5]],
                     index=['blue','green','red'],
                     columns=['ball','mug','pen'])
frame3



frame3.dropna()


frame3.dropna(how='all')


# ### Filliing in NaN Occurrences


frame3.fillna(0)


frame3.fillna({'ball':1, 'mug':0, 'pen': 99})



























# ## Lecture 11. Hierarchical Indexing and Leveling of Pandas Data Structures

import pandas as pd
import numpy as np

mser = pd.Series(np.random.rand(8),
                index=[['white','white','white','blue','blue','red','red','red'],
                      ['up','down','right','up','down','up','down','left']])
mser


mser.index


mser['white']


mser[:,'up']


mser['white','up']


mser.unstack()


frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame


frame.stack()


mframe = pd.DataFrame(np.random.randn(16).reshape(4,4),
                     index=[['white','white','red','red'], ['up','down','up','down']],
                     columns=[['pen','pen','paper','paper'],[1,2,1,2]])
mframe


# # Reordering and Sorting Levels


mframe.columns.names = ['objects','id']
mframe.index.names = ['colors','status']
mframe


mframe.swaplevel('colors','status')


mframe.sort_index(level='colors')


# ### Summary Statistic by Level


mframe.sum(level='colors')


mframe.sum(level='id',axis=1)










# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""


### Section 12. Reading and Writing Data with Pandas library
##Lecture 1. Reading Data in CSV or Text Files with Pandas

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


csvframe = pd.read_csv('sec12_01.csv')
csvframe


pd.read_table('sec12_01.csv', sep=',')



pd.read_csv('sec12_02.csv', header=None)


pd.read_csv('sec12_02.csv', names=['white','red','blue','green','animal'])


pd.read_csv('sec12_03.csv', index_col=['color','status'])




# ### Reading TXT Files into Parts or Partially

pd.read_csv('sec12_02.csv',skiprows=[2],nrows=3,header=None)



































# ### Lecture 2. Using Regular Expressions to Parse TXT Files with Pandas

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


pd.read_table('sec12_04.txt', sep='\s+', engine='python')



pd.read_table('sec12_05.txt', sep='\D+', header=None, engine='python')


























# ### Lecture 3.Writing Data in CSV with Pandas

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


frame = pd.DataFrame(np.arange(16).reshape((4,4)),
          index=['red', 'blue', 'yellow', 'white'],
          columns=['ball','pen','pencil','paper'])
frame


frame.to_csv('sec12_07.csv')



frame.to_csv('sec12_07b.csv', index=False, header=False)



frame3 = pd.DataFrame([[6,np.nan,np.nan,6,np.nan],
              [np.nan,np.nan,np.nan,np.nan,np.nan],
              [np.nan,np.nan,np.nan,np.nan,np.nan],
              [20,np.nan,np.nan,20.0,np.nan],
              [19,np.nan,np.nan,19.0,np.nan]
             ],
                     index=['blue','green','red','white','yellow'],
                     columns=['ball','mug','paper','pen','pencil'])


frame3.to_csv('sec12_08.csv')


frame3.to_csv('sec12_09.csv', na_rep = 'NaN')




















# ## Lecture 4.Reading and Writing Data on Microsoft Excel Files with Pandas

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


pd.read_excel('sec12_data.xlsx',index_col=0)

pd.read_excel('sec12_data.xlsx', 'Sheet2',index_col=0)

pd.read_excel('sec12_data.xlsx', 1 ,index_col=0)


#write to excel


frame = pd.DataFrame(np.random.random((4,4)),
                    index = ['exp1','exp2','exp3','exp4'],
                    columns = ['Jan2015','Feb2015','Mar2015','Apr2015'])
frame

frame.to_excel('sec12_data02.xlsx')



























# ## Lecture 5. Reading and Writing HTML Files with Pandas

# ### Writing Data in HTML

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


frame = pd.DataFrame(np.arange(4).reshape(2,2))
print(frame.to_html())



frame = pd.DataFrame( np.random.random((4,4)),
                    index = ['white','black','red','blue'],
                    columns = ['up','down','right','left'])
frame


s = ['<HTML>']
s.append('<HEAD><TITLE>My DataFrame</TITLE></HEAD>')
s.append('<BODY>')
s.append(frame.to_html())
s.append('</BODY></HTML>')
html = ''.join(s)


html_file = open('myFrame.html','w')
html_file.write(html)
html_file.close()


# ### Reading Data from an HTML File

web_frames = pd.read_html('myFrame.html')
web_frames[0]



ranking = pd.read_html('https://www.meccanismocomplesso.org/en/meccanismo-complesso-sito-2/classifica-punteggio/')

ranking[0]























# ### Lecture6. Reading Data from XML

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


from lxml import objectify
xml = objectify.parse('books.xml')
xml


root = xml.getroot()

root.Book.Author


root.Book.PublishDate


root.getchildren()



[child.tag for child in root.Book.getchildren()]



[child.text for child in root.Book.getchildren()]





def etree2df(root):
    column_names = []
    for i in range(0, len(root.getchildren()[0].getchildren())):
        column_names.append(root.getchildren()[0].getchildren()[i].tag)
    xmlframe = pd.DataFrame(columns=column_names)
    for j in range(0, len(root.getchildren())):
        obj = root.getchildren()[j].getchildren()
        texts = []
        for k in range(0, len(column_names)):
            texts.append(obj[k].text)
        row = dict(zip(column_names, texts))
        row_s = pd.Series(row)
        row_s.name = j
        xmlframe = xmlframe.append(row_s)
    return xmlframe


etree2df(root)

























# ## Lecture 7.Reading and Writing JSON Data with Pandas

import numpy as np
import pandas as pd

#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

#writing to a json file
frame = pd.DataFrame(np.arange(16).reshape(4,4),
                    index=['white','black','red','blue'],
                    columns=['up','down','right','left'])
frame.to_json('frame.json')



##reading json file
pd.read_json('frame.json')


from pandas.io.json import json_normalize



file = open('books.json','r')
text = file.read()
text = pd.io.json.loads(text)



pd.json_normalize(text,'books')



pd.json_normalize(text,'books',['writer','nationality'])





















"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""


### Section 13. pandas in Depth: Data Manipulation
##Lecture 1. Merging Datasets with Pandas

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


frame1 = pd.DataFrame( {'id':['ball','pencil','pen','mug','ashtray'],
                        'price': [12.33,11.44,33.21,13.23,33.62]})
frame1

frame2 = pd.DataFrame( {'id': ['pencil','pencil','ball','pen'],
                        'color':['white','red','red','black']})
frame2

pd.merge(frame1,frame2)


frame1 = pd.DataFrame( {'id': ['ball','pencil','pen',',mug','ashtray'],
                        'color': ['white','red','red','black','green'],
                        'brand': ['OMG','ABC','ABC','POD','PPOD']})
frame1

frame2 = pd.DataFrame( {'id':['pencil','pencil','ball','pen'],
                        'brand': ['OMG','POD','ABC','POD']})
frame2


pd.merge(frame1,frame2)


pd.merge(frame1, frame2, on='id')


pd.merge(frame1, frame2, on='brand')


frame2.columns = ['sid', 'brand']
frame2


pd.merge(frame1, frame2, left_on='id', right_on='sid')



frame2.columns = ['id','brand']
pd.merge(frame1,frame2,on='id')



pd.merge(frame1,frame2,on='id',how='outer')


pd.merge(frame1,frame2,on='id',how='left')


pd.merge(frame1,frame2,on='id',how='right')


pd.merge(frame1,frame2,on=['id','brand'], how='outer')


# ### Merging on Index

pd.merge(frame1,frame2,right_index=True, left_index=True)

pd.merge(frame1,frame2,right_index=True, left_index=True, how='outer')


frame1.join(frame2)     #error




frame2.columns = ['id2','brand2']
frame1.join(frame2)

















# ## Lecture 2.Concatenating Datasets with Numpy and Pandas
import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


#concatenating Numpy arrays
array1 = np.arange(9).reshape((3,3))
array1

array2 = np.arange(9).reshape((3,3))+6
array2


np.concatenate([array1,array2],axis=1)


np.concatenate([array1,array2],axis=0)




#concatenating Pandas Series and Dataframes
ser1 = pd.Series(np.random.rand(4), index=[1,2,3,4])
ser1


ser2 = pd.Series(np.random.rand(4), index=[5,6,7,8])
ser2


pd.concat([ser1,ser2])


pd.concat([ser1,ser2], axis=1)



pd.concat([ser1,ser2], keys=[1,2])



pd.concat([ser1,ser2], axis=1, keys=[1,2])



frame1 = pd.DataFrame(np.random.rand(9).reshape(3,3), index=[1,2,3], columns=['A','B','C'])
frame2 = pd.DataFrame(np.random.rand(9).reshape(3,3), index=[4,5,6], columns=['A','B','C'])
pd.concat([frame1, frame2])


pd.concat([frame1, frame2], axis=1)



# ### Combining datasets


ser1 = pd.Series(np.random.rand(5), index=[1,2,3,4,5])
ser1


ser2 = pd.Series(np.random.rand(4), index=[2,4,5,6])
ser2


ser1.combine_first(ser2)


ser2.combine_first(ser1)


ser1[:3].combine_first(ser2[:3])




















# ## Lecture 3. Pivoting,Stacking,Unstacking,Long and Wide forms of Datasets with Pandas
import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

# ### Pivoting with Hierarchical Indexing

frame1 = pd.DataFrame(np.arange(9).reshape(3,3),
                     index=['white','black','red'],
                     columns=['ball','pen','pencil'])
frame1


ser = frame1.stack()
ser


ser.unstack()


ser.unstack(0)


# ### Pivoting from "Long" to "Wide" Format

longframe = pd.DataFrame({ 'color':['white','white','white',
                                    'red','red','red',
                                    'black','black','black'],
                           'item':['ball','pen','mug',
                                   'ball','pen','mug',
                                   'ball','pen','mug'],
                           'value': np.random.rand(9)})
longframe


wideframe = longframe.pivot('color','item')
wideframe






















# ### Lecture 4.Removing, Mapping Operations with Pandas

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


#removing rows or columns

frame1 = pd.DataFrame(np.arange(9).reshape(3,3),
                       index=['white','black','red'],
                       columns=['ball','pen','pencil'])
frame1



del frame1['ball']
frame1



frame1.drop('white')



# ### Removing Duplicate rows

dframe = pd.DataFrame({ 'color': ['white','white','red','red','white'],
                        'value': [2,1,3,3,2]})
dframe


dframe.duplicated()


dframe[dframe.duplicated()]


dframe.drop_duplicates()


# ### Replacing Values via Mapping

frame = pd.DataFrame({ 'item':['ball','mug','pen','pencil','ashtray'],
                       'color':['white','rosso','verde','black','yellow'],
                       'price':[5.56,4.20,1.30,0.56,2.75]})
frame


newcolors = {
    'rosso': 'red',
    'verde': 'green'
}


frame.replace(newcolors)


ser = pd.Series([1,3,np.nan,4,6,np.nan,3])
ser


ser.replace(np.nan,0)


# ### Adding Values via Mapping


frame = pd.DataFrame({'item':['ball','mug','pen','pencil','ashtray'],
                      'color':['white','red','green','black','yellow']})
frame


price = {
    'ball': 5.56,
    'mug': 4.20,
    'bottle': 1.30,
    'scissors': 3.41,
    'pen': 1.30,
    'pencil': 0.56,
    'ashtray': 2.75
}



frame['price'] = frame['item'].map(price)
frame






















# ### Lecture 5.Rename the Indexes of the Axes

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


frame = pd.DataFrame({'item':['ball','mug','pen','pencil','ashtray'],
                      'color':['white','red','green','black','yellow'],
                      'price':[5.56,4.2,1.3,0.56,2.75]})

frame


reindex = {
    0: 'first',
    1: 'second',
    2: 'third',
    3: 'fourth',
    4: 'fifth'
}
frame.rename(reindex)


recolumn = {
    'item': 'object',
    'price': 'value'
}
frame.rename(index=reindex, columns=recolumn)


frame.rename(index={1:'first'}, columns={'item':'object'})


frame.rename(columns={'item':'object'}, inplace=True)


frame
























# ### Lecture 6. Detecting and Filtering Outliers

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


randframe = pd.DataFrame(np.random.randn(1000,3))
randframe.describe()


randframe.std()

#show the outlier, if any column has outlier value
randframe[(np.abs(randframe) > (3*randframe.std())).any(1)]


#to get the data excluding outlier observations
outindex= np.array(randframe[(np.abs(randframe) > (3 *randframe.std())).any(1)].index)
outindex

randframe.drop(outindex, inplace=True)

len(randframe)



#show the outlier observation, if one specified column has outlier
randframe = pd.DataFrame(np.random.randn(1000,3))

randframe[randframe[2] > 2.8 *randframe[2].std()]

randframe[randframe[2] <= 2.8*randframe[2].std()]













# ## Lecture 7. Discretization and Binning of Datasets with Pandas

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory



results = [12,34,67,55,28,90,99,12,3,56,74,44,87,23,49,89,87]
bins = [0,25,50,75,100]
cat = pd.cut(results, bins)
cat


cat.categories


cat.codes



pd.value_counts(cat)



bin_names = ['unlikely','less likely','likely','highly likely']
pd.cut(results, bins, labels=bin_names)


# cut into 5 bins in equal interval
pd.cut(results, 5)



#qcut will make the number of occurrence in each bin equal
quintiles = pd.qcut(results, 5)
quintiles


pd.value_counts(quintiles)




#create a bin column for a dataframe
df = pd.DataFrame({'number': np.random.randint(1, 100, 10)})
df['bins'] = pd.cut(x=df['number'], bins=[1, 20, 40, 60, 80, 100],
                    labels=['1', '2', '3', '4', '5'])

print(df)
 





















# ## Lecture 8. Permutation,Random Sampling with Pandas

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


#Permutation
nframe = pd.DataFrame(np.arange(25).reshape(5,5))
nframe


new_order = np.random.permutation(5)
new_order


nframe.take(new_order)


new_order = [3,4,2]
nframe.take(new_order)


# ### Random Sampling


sample = np.random.randint(0, len(nframe), size=3)
sample


nframe.take(sample)



























# ## Lecture 9.Data Aggregation and Grouping with Pandas

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

#Data aggregation

frame = pd.DataFrame({ 'color': ['white','red','green','red','green'],
                       'object': ['pen','pencil','pencil','ashtray','pen'],
                       'price1': [5.56, 4.20, 1.30, 0.56, 2.75],
                       'price2': [4.75,4.12,1.60,0.75,3.15]})
frame


group = frame['price1'].groupby(frame['color'])
group


group.groups


group.mean()


group.sum()


# ### Hierarchical Grouping

ggroup = frame['price1'].groupby([frame['color'],frame['object']])
ggroup.groups


ggroup.sum()


frame[['price1','price2']].groupby(frame['color']).mean()


frame.groupby(frame['color']).mean()


# ## Group Iteration

for name, group in frame.groupby('color'):
    print(name)
    print(group)


# ### Chain of Transformations
result1 = frame['price1'].groupby(frame['color']).mean()
type(result1)


result2 = frame.groupby(frame['color']).mean()
type(result2)



frame['price1'].groupby(frame['color']).mean()


frame.groupby(frame['color'])['price1'].mean()


(frame.groupby(frame['color']).mean())['price1']


means = frame.groupby('color').mean().add_prefix('mean_')
means


# ### Functions on Groups

group = frame.groupby('color')
group['price1'].quantile(0.6)


def range(series):
    return series.max() - series.min()
group['price1'].agg(range)


group.agg(range)


group['price1'].agg(['mean','std',range])

















### Lecture 10.  Reshape Wide long form pandas dataframe
### using stack,unstack and melt method

import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory


## stack() - from dataframe to Series
# ### Pivoting from a dataframe to Series with Hierarchical Indexing

frame1 = pd.DataFrame(np.arange(9).reshape(3,3),
                     index=['white','black','red'],
                     columns=['ball','pen','pencil'])
frame1


ser = frame1.stack()
ser



### unstack() - from Series to dataframe
## to return as a dataframe

ser.unstack()


ser.unstack(0)


# ### Pivoting - from "Long" to "Wide" Format

longframe = pd.DataFrame({ 'color':['white','white','white',
                                    'red','red','red',
                                    'black','black','black'],
                           'item':['ball','pen','mug',
                                   'ball','pen','mug',
                                   'ball','pen','mug'],
                           'value': np.random.rand(9)})
longframe


wideframe = longframe.pivot('color','item')
wideframe

wideframe.reset_index(inplace = True)
wideframe


# melt()  - from wide format to long form
frame = pd.DataFrame({'item':['ball','mug','pen','pencil','ashtray'],
                      'color':['white','red','green','black','yellow'],
                      'size' : ['large','medium','small','big','small'],
                      'price':[5.56,4.2,1.3,0.56,2.75]})

frame

df_melt = frame.melt(id_vars =['item', 'color']) 

df_melt


wideframe = df_melt.pivot(index=['item','color'], columns='variable')
wideframe


wideframe.reset_index(inplace = True)

wideframe






    


# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""


#### Section 14. Data visualization with matplotlib

## Lecture 1. Getting started using matplotlib

import matplotlib.pyplot as plt

#plot a line
plt.plot([1,2,3,4])
plt.show()


# ## Set the properties of the plot

plt.plot([1,2,3,4],[1,4,9,16],'ro')

plt.show()


#sett axis range and title

plt.axis([0,5,0,20])
plt.title('My first plot')
plt.plot([1,2,3,4],[1,4,9,16],'ro')
#plt.plot([1,2,3,4],[1,4,9,16])
plt.show()

# ### matplotlib and NumPy

import math
import numpy as np

t = np.arange(0,2.5,0.1)
y1 = np.sin(math.pi*t)
y2 = np.sin(math.pi*t+math.pi/2)
y3 = np.sin(math.pi*t-math.pi/2)
plt.plot(t,y1,'b*',t,y2,'g^',t,y3,'ys')
plt.show()



plt.plot(t,y1,'b',t,y2,'g',t,y3,'r')
plt.show()

plt.plot(t,y1,'b--',t,y2,'g',t,y3,'r-.')
plt.show()



# ## Using the kwargs
#The objects that make up a chart have many attributes that 
#characterize them. These attributes are all default values, 
#but can be set through the use of keyword args, often
#referred as kwargs.

#set thickness
plt.plot([1,2,4,2,1,0,1,2,1,4], linewidth=2.0)
plt.show()


# ### Working with multiple Figures and Axes

#matplotlib allows you to manage multiple figures simultaneously, 
#and within each figure, it offers the ability to view different
#plots defined as subplots

#The argument of the subplot() function is composed of three integers.
# The first number defines how many parts the figure is split
#into vertically. The second number defines how many parts the 
#figure is divided into horizontally. The third issue selects 
#which is the current subplot on which you can direct commands.

#plot sin and cos in two subplots vertically
t = np.arange(0,5,0.1)
y1 = np.sin(2*np.pi*t)
y2 = np.sin(2*np.pi*t)
plt.subplot(211)
plt.plot(t,y1,'b-.')
plt.subplot(212)
plt.plot(t,y2,'r--')
plt.show()


#plot sin and cos in two subplots horizontally
t = np.arange(0.,1.,0.05)
y1 = np.sin(2*np.pi*t)
y2 = np.cos(2*np.pi*t)
plt.subplot(121)
plt.plot(t,y1,'b-.')
plt.subplot(122)
plt.plot(t,y2,'r--')
plt.show()














### lecture 2. Adding text to charts in matplotlib
import matplotlib.pyplot as plt
import math
import numpy as np

# ### Adding text

#add x,y axis labels
plt.axis([0,5,0,20])
plt.title('My first plot')
plt.xlabel('Counting')
plt.ylabel('Square values')
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.show()


#modify the title by changing the font and increasing the size of 
#the character, and modify the color of the axis labels
plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.show()


# text() allows you to add text to any position within a chart.
#text(x,y,s, fontdict=None, **kwargs)
#The first two arguments are the coordinates of the location 
#where you want to place the text. s is the string of text to 
#be added, and fontdict (optional) is the font that you
#want to use. Finally, you can add the keywords.

#Add the label to each point of the plot

plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.show()


# matplotlib offers the possibility to integrate LaTeX expressions
#mostly usef for mathematical expressions.
#To do this, you can add a LaTeX expression to the text, enclosing 
#it between two $ characters. The interpreter will recognize them 
#as LaTeX expressions and convert them to the corresponding graphic, 
#which can be a mathematical expression, a formula,mathematical 
#characters, or just Greek letters. Generally you have to precede 
#the string containing LaTeX expressions with an r, which indicates
# raw text, in order to avoid unintended escape sequences

#add a formula describing the trend followed by the point 
#of the plot and enclose it in a colored bounding box
plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.text(1.1,12,r'$y = x^2$', fontsize=20, bbox={'facecolor':'yellow','alpha':0.2})         
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.show()


















# ### Lecture 3. Adding a grid in matplotlib
#Grid lets you better understand the position occupied by 
#each point on the chart

import matplotlib.pyplot as plt
import math
import numpy as np


plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.text(1.1,12,r'$y = x^2$', fontsize=20, bbox={'facecolor':'yellow','alpha':0.2})  
plt.grid(True)
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.show()




x = np.arange(-5, 5, 0.01)
y = np.sin(2*np.pi*x)
plt.plot(x, y)
# Specify grid with line attributes
plt.grid(color='r', linestyle='--')
plt.show()




















# ### Lecture 4. Adding Legend to charts in matplotlib
#Add a legend to your chart with the legend() function and a string
#indicating the words with which you want the series to be shown
import matplotlib.pyplot as plt
import math
import numpy as np

#assign the First series name to the input data array
plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.text(1.1,12,r'$y = x^2$', fontsize=20, bbox={'facecolor':'yellow','alpha':0.2})  
plt.grid(True)
plt.plot([1,2,3,4],[1,4,9,16],'ro')
#A legend is added in the upper-right corner by default
plt.legend(['First series'])
plt.show()




#the position occupied by the legend is set by assigning numbers 
#from 0 to 10 to the loc kwarg. Each of these numbers characterizes 
#one of the corners of the chart
#0 best
#1 upper-right
#2 upper-left
#3 lower-right
#4 lower-left
#5 right
#6 center-left
#7 center-right
#8 lower-center
#9 upper-center
#10 center

#move the legend in the upper-left corner
plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.text(1.1,12,r'$y = x^2$', fontsize=20, bbox={'facecolor':'yellow','alpha':0.2})  
plt.grid(True)
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.plot([1,2,3,4],[0.8,3.5,8,15],'g^')
plt.plot([1,2,3,4],[0.5,2.5,4,12],'b*')
plt.legend(['First series','Second series','Third series'], loc=2)
plt.show()

















#### Lecture 5. Save your charts in matplotlib
import matplotlib.pyplot as plt
import math
import numpy as np

import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

# ### Saving your chart directly as an Image

plt.axis([0,5,0,20])
plt.title('My first plot', fontsize=20, fontname='Times New Roman')
plt.xlabel('Counting', color='gray')
plt.ylabel('Square values',color='gray')
plt.text(1,1.5,'First')
plt.text(2,4.5,'Second')
plt.text(3,9.5,'Third')
plt.text(4,16.5,'Fourth')
plt.text(1.1,12,r'$y = x^2$', fontsize=20, bbox={'facecolor':'yellow','alpha':0.2})  
plt.grid(True)
plt.plot([1,2,3,4],[1,4,9,16],'ro')
plt.plot([1,2,3,4],[0.8,3.5,8,15],'g^')
plt.plot([1,2,3,4],[0.5,2.5,4,12],'b*')
plt.legend(['First series','Second series','Third series'], loc=2)
plt.savefig('my_chart.png')















### Lecture 6. Handling Date Values of charts in matplotlib
import datetime
import numpy as np
import matplotlib.pyplot as plt

#problematic showing date ticks on x-axis using default setting
#for day-month-year
events = [datetime.date(2015,1,23), 
          datetime.date(2015,1,28),
          datetime.date(2015,2,3),
          datetime.date(2015,2,21),
          datetime.date(2015,3,15),
          datetime.date(2015,3,24),
          datetime.date(2015,4,8),
          datetime.date(2015,4,24)]
readings = [12,22,25,20,18,15,17,14]
plt.plot(events,readings)
plt.show()



#To manage dates it is therefore advisable to define a time scale 
#with appropriate  objects. First you need to import matplotlib.dates,
#a module specialized for this type of data. Then you define the 
#scales of the times, as in this case, a scale of days and one of
#the months, through the MonthLocator() and DayLocator() functions. 
#In these cases, the formatting is also very important, and to avoid 
#overlap or unnecessary references,you have to limit the tick labels 
#to the essential, which in this case is year-month. This format 
#can be passed as an argument to the DateFormatter() function. After
#you defined the two scales, one for the days and one for the months, 
#you can set two different kinds of ticks on the x-axis, using the 
#set_major_locator() and set_minor_locator() functions on the 
#xaxis object. To set the text format of the tick labels referred 
#to the months you have to use the set_major_formatter() function.


import matplotlib.dates as mdates

months = mdates.MonthLocator()
days = mdates.DayLocator()
timeFmt = mdates.DateFormatter('%Y-%m')
events = [datetime.date(2015,1,23), 
          datetime.date(2015,1,28),
          datetime.date(2015,2,3),
          datetime.date(2015,2,21),
          datetime.date(2015,3,15),
          datetime.date(2015,3,24),
          datetime.date(2015,4,8),
          datetime.date(2015,4,24)]
readings = [12,22,25,20,18,15,17,14]
fig, ax = plt.subplots()
plt.plot(events, readings)
ax.xaxis.set_major_locator(months)
ax.xaxis.set_major_formatter(timeFmt)
ax.xaxis.set_minor_locator(days)
plt.show()














### Line charts with matplotlib
import datetime
import numpy as np
import matplotlib.pyplot as plt

#A mathematical function represented in a line chart
x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
plt.plot(x,y)
plt.show()



#display a family of functions:
#a different color is automatically assigned to each line.
#All the plots are represented on the same scale; #that is, the data
#points of each series refer to the same x-axis and y-axis.
x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
y2 = np.sin(2*x)/x
y3 = np.sin(x)/x
plt.plot(x,y)
plt.plot(x,y2)
plt.plot(x,y3)
plt.show()



#specify color and styles for lines

x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
y2 = np.sin(2*x)/x
y3 = np.sin(x)/x
plt.plot(x,y,'k--',linewidth=3)
plt.plot(x,y2,'m-.')
plt.plot(x,y3,color='#87a3cc',linestyle='--')
plt.show()


##Using tick labels in LaTeX format

#by default, values on ticks are shown in numerical form. 
#you replace the numerical values with multiple of π. 
#You can also replace the ticks on the y-axis. 
#To do all this, you have to use xticks() and yticks() functions.
#passing to each of them two lists of values. 
#The first list contains values corresponding to the positions 
#where the ticks are to be placed,and the second contains the 
#tick labels. 
#you can use strings containing LaTeX format in order to correctly 
#display the symbol π. 
#For LaTex, you need to define them within two $ characters 
#and to add a r as the prefix.


x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
y2 = np.sin(2*x)/x
y3 = np.sin(x)/x
plt.plot(x,y,color='b')
plt.plot(x,y2,color='r')
plt.plot(x,y3,color='g')
plt.xticks([-2*np.pi,-np.pi,0, np.pi, 2*np.pi],
           [r'$-2\pi$',r'$-\pi$',r'$0$',r'$+\pi$',r'$+2\pi$'])
plt.yticks([-1,0,1,2,3],
           [r'$-1$',r'$0$',r'$+1$',r'$+2$',r'$+3$'])
plt.show()



#to have the two axes passing through the origin (0, 0)
#the two axes crossing in the middle of the figure

#To do this, you must first capture the Axes object through the 
#gca() function. Then through this object, you can select each of 
#the four sides making up the bounding box,specifying for each one 
#its position: right, left, bottom, and top. Crop the sides that 
#do not match any axis (right and top) using the set_color() 
#function and indicating none for color

x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
y2 = np.sin(2*x)/x
y3 = np.sin(x)/x
plt.plot(x,y,color='b')
plt.plot(x,y2,color='r')
plt.plot(x,y3,color='g')
plt.xticks([-2*np.pi,-np.pi,0, np.pi, 2*np.pi],
           [r'$-2\pi$',r'$-\pi$',r'$0$',r'$+\pi$',r'$+2\pi$'])
plt.yticks([-1,0,1,2,3],
           [r'$-1$',r'$0$',r'$+1$',r'$+2$',r'$+3$'])
ax = plt.gca()
ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')
#representing the x-axis
ax.xaxis.set_ticks_position('bottom')
ax.spines['bottom'].set_position(('data',0))
#representing the y_axis
ax.yaxis.set_ticks_position('left')
ax.spines['left'].set_position(('data',0))
plt.show()



#to specify a particular point of the line using a notation 
#and optionally add an arrow to better indicate the position
#of the point

#using function annotate(), 
#The first argument is the string to be represented containing 
#the expression in LaTeX; 
#The point of the chart to note is indicated by a list containing 
#the coordinates of the point [x, y]. 
#The distance of the textual notation from the point to be highlighted 
#is defined by the xytext kwarg, and represented by means of a curved 
#arrow

#get the chart with the mathematical notation of the limit

x = np.arange(-2*np.pi,2*np.pi,0.01)
y = np.sin(3*x)/x
y2 = np.sin(2*x)/x
y3 = np.sin(x)/x
plt.plot(x,y,color='b')
plt.plot(x,y2,color='r')
plt.plot(x,y3,color='g')
plt.xticks([-2*np.pi,-np.pi,0, np.pi, 2*np.pi],
           [r'$-2\pi$',r'$-\pi$',r'$0$',r'$+\pi$',r'$+2\pi$'])
plt.yticks([-1,0,1,2,3],
           [r'$-1$',r'$0$',r'$+1$',r'$+2$',r'$+3$'])
plt.annotate(r'$\lim_{x\to 0}\frac{\sin(x)}{x}= 1$', xy=[0,1],xycoords='data',
             xytext=[30,30],fontsize=16, textcoords='offset points', arrowprops=dict(arrowstyle="->",
             connectionstyle="arc3,rad=.2"))
ax = plt.gca()
ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')
ax.xaxis.set_ticks_position('bottom')
ax.spines['bottom'].set_position(('data',0))
ax.yaxis.set_ticks_position('left')
ax.spines['left'].set_position(('data',0))
plt.show()


# ## Line Charts with pandas

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
data = {'series1':[1,3,4,3,5],
        'series2':[2,4,5,2,4],
        'series3':[3,2,3,1,3]}
df = pd.DataFrame(data)
x = np.arange(5)
plt.axis([0,5,0,7])
plt.plot(x,df)
plt.legend(data, loc=2)
plt.show()






















# ## Lecture 8.Histograms using matplotlib in Python

import matplotlib.pyplot as plt
import numpy as np

#simple example
# Creating dataset
test = np.array([25, 57, 15, 93, 34,
              73, 54, 54, 11,
              20, 51, 52, 75, 31,
              27, 39,29])
 
# Creating histogram, setting the bin bounding
fig, ax = plt.subplots(figsize =(12, 9))
ax.hist(test, bins = [0, 20, 40, 60, 80, 100])
 
# Show plot
plt.show()



#setting bin number
test2 = np.random.randint(0,200,200)
test2


n, bin, patches = plt.hist(test2, bins=20)
plt.show()

n
bin


#Customization of Histogram
#different color for different bins
#adjust axis
#add text


import matplotlib.pyplot as plt
import numpy as np
from matplotlib import colors
from matplotlib.ticker import PercentFormatter
 
 
# Creating dataset
np.random.seed(23685752)
N_points = 10000
n_bins = 20
 
# Creating distribution
x = np.random.randn(N_points)
y = .8 ** x + np.random.randn(10000) + 25
legend = ['distribution']

# Creating histogram
fig, axs = plt.subplots(1, 1,
                        figsize =(10, 7),
                        )
 
 
# Remove axes splines
for s in ['top', 'bottom', 'left', 'right']:
    axs.spines[s].set_visible(False)
 
# Remove x, y ticks
axs.xaxis.set_ticks_position('none')
axs.yaxis.set_ticks_position('none')
   
# Add padding between axes and labels
axs.xaxis.set_tick_params(pad = 5)
axs.yaxis.set_tick_params(pad = 10) 

# Add x, y gridlines
axs.grid(b = True, color ='grey',
        linestyle ='-.', linewidth = 0.5,
        alpha = 0.6)
 
# Add Text watermark
fig.text(0.9, 0.15, 'Jeeteshgavande30',
         fontsize = 12,
         color ='red',
         ha ='right',
         va ='bottom',
         alpha = 0.7)

# Creating histogram
N, bins, patches = axs.hist(x, bins = n_bins)
 
# Setting color
fracs = ((N**(1 / 5)) / N.max())
norm = colors.Normalize(fracs.min(), fracs.max())
 
for thisfrac, thispatch in zip(fracs, patches):
    color = plt.cm.viridis(norm(thisfrac))
    thispatch.set_facecolor(color)
 
# Adding extra features   
plt.xlabel("X-axis")
plt.ylabel("y-axis")
plt.legend(legend)
plt.title('Customized histogram')
 
# Show plot
plt.show()




















# ## Lecture 9. Bar Charts using matplotlib in Python

import matplotlib.pyplot as plt
# a simple bar chart in which indices are drawn on the x-axis
index = [0,1,2,3,4]
values = [5,7,3,4,6]
plt.bar(index,values)
plt.show()



#specify the categories through the tick label
#by passing a  list of strings to the xticks() function

import matplotlib.pyplot as plt
import numpy as np

index = np.arange(5)
values1 = [5,7,3,4,6]
plt.bar(index, values1)
plt.xticks(index+0.4,['A','B','C','D','E'])
plt.show()





import matplotlib.pyplot as plt
import numpy as np

#bar chart with error bar
#refine the bar chart, adding standard deviations of each bar,through
#the yerr kwarg along with a list containing the standard deviations

index = np.arange(5)
values1 = [5,7,3,4,6]
std1 = [0.8,1,0.4,0.9,1.3]
plt.title('A Bar Chart')
plt.bar(index, values1, yerr=std1, error_kw={'ecolor':'0.1','capsize':6},alpha=0.7,label='First')
plt.xticks(index+0.4,['A','B','C','D','E'])
plt.legend(loc=2)
plt.show()


# ### Horizontal Bar Chart
#barh(). The arguments and the kwargs valid for the bar() function 
#remain the same for this function.The only change that the categories
# are represented on the y-axis and the numerical values are
#shown on the x-axis now
import matplotlib.pyplot as plt
import numpy as np

index = np.arange(5)
values1 = [5,7,3,4,6]
std1 = [0.8,1,0.4,0.9,1.3]
plt.title('A Horizontal Bar Chart')
plt.barh(index, values1, xerr=std1, error_kw={'ecolor':'0.1','capsize':6},alpha=0.7,label='First')
plt.yticks(index+0.4,['A','B','C','D','E'])
plt.legend(loc=5)
plt.show()


# ### Multiserial Bar Chart
#to define a sequence of indexes, each corresponding to a bar, 
#to be assigned to the x-axis. These indices should represent
# categories.
import matplotlib.pyplot as plt
import numpy as np

index = np.arange(5)
values1 = [5,7,3,4,6]
values2 = [6,6,4,5,7]
values3 = [5,6,5,4,6]
bw = 0.3
plt.axis([0,5,0,8])
plt.title('A Multiseries Bar Chart', fontsize=20)
plt.bar(index, values1, bw, color='b')
plt.bar(index+bw, values2, bw, color='g')
plt.bar(index+2*bw, values3, bw, color='r')
plt.xticks(index+1.5*bw,['A','B','C','D','E'])
plt.show()


# Multiserial hotizontal Bar Chart
#have to reverse the axis range value
#using yticks
import matplotlib.pyplot as plt
import numpy as np

index = np.arange(5)
values1 = [5,7,3,4,6]
values2 = [6,6,4,5,7]
values3 = [5,6,5,4,6]
bw = 0.3
plt.axis([0,8,0,5])
plt.title('A Multiseries Bar Chart', fontsize=20)
plt.barh(index, values1, bw, color='b')
plt.barh(index+bw, values2, bw, color='g')
plt.barh(index+2*bw, values3, bw, color='r')
plt.yticks(index+0.4,['A','B','C','D','E'])
plt.show()


# ### Multiseries Bar Chart with pandas DataFrame

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

index = np.arange(5)
data = {'series1': [1,3,4,3,5],
        'series2': [2,4,5,2,4],
        'series3': [3,2,3,1,3]}
df = pd.DataFrame(data)
df.plot(kind='bar')
plt.show()


# Multiseries horizontal Bar Chart with pandas DataFrame

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

index = np.arange(5)
data = {'series1': [1,3,4,3,5],
        'series2': [2,4,5,2,4],
        'series3': [3,2,3,1,3]}
df = pd.DataFrame(data)
df.plot(kind='barh')
plt.show()


# ### Multiseries Stacked Bar Charts

# bars are stacked one on the other, by adding the bottom
#kwarg to each bar() function, and Each series must be assigned 
#to the corresponding bottom kwarg.

import matplotlib.pyplot as plt
import numpy as np

series1 = np.array([3,4,5,3])
series2 = np.array([1,2,2,5])
series3 = np.array([2,3,3,4])
index = np.arange(4)
plt.axis([-0.5,3.5,0,15])
plt.title('A Multiseries Stacked Bar Chart')
plt.bar(index,series1,color='r')
plt.bar(index,series2,color='b',bottom=series1)
plt.bar(index,series3,color='g',bottom=(series2+series1))
plt.xticks(index,['Jan18','Feb18','Mar18','Apr18'])
plt.show()


# Multiseries horizontal Stacked Bar Charts


import matplotlib.pyplot as plt
import numpy as np

series1 = np.array([3,4,5,3])
series2 = np.array([1,2,2,5])
series3 = np.array([2,3,3,4])
index = np.arange(4)
plt.axis([0,15,-0.5,3.5])
plt.title('A Multiseries Horizontal Stacked Bar Chart')
plt.barh(index,series1,color='r')
plt.barh(index,series2,color='b',left=series1)
plt.barh(index,series3,color='g',left=(series2+series1))
plt.yticks(index,['Jan18','Feb18','Mar18','Apr18'])
plt.show()


# use hatches that allow you to fill the various bars with 
#strokes drawn in a different way
#To do this, you have first to set the color of the bar as white 
#and then you have to use the hatch kwarg to define how the hatch 
#is to be set. The various hatches have codes distinguishable 
#among these characters (|, /, -, \, *, -) corresponding to the 
#line style filling the bar. The more a symbol is replicated, 
#the denser the lines forming the hatch will be. For example, 
#/// is more dense than //, which is more dense than /
import matplotlib.pyplot as plt
import numpy as np

series1 = np.array([3,4,5,3])
series2 = np.array([1,2,2,5])
series3 = np.array([2,3,3,4])
index = np.arange(4)
plt.axis([0,15,-0.5,3.5])
plt.title('A Multiseries Horizontal Stacked Bar Chart')
plt.barh(index,series1,color='w',hatch='xx')
plt.barh(index,series2,color='w',hatch='///',left=series1)
plt.barh(index,series3,color='w',hatch='\\\\\\',left=(series2+series1))
plt.yticks(index,['Jan18','Feb18','Mar18','Apr18'])
plt.show()


# ### Stacked Bar Charts with pandas DataFrame

import matplotlib.pyplot as plt
import pandas as pd

data = {'series1': [1,3,4,3,5],
        'series2': [2,4,5,2,4],
        'series3': [3,2,3,1,3]}
df = pd.DataFrame(data)
df.plot(kind='bar',stacked=True)
plt.show()























# ## Lecture 10. Pie Charts using matplotlib in Python

import matplotlib.pyplot as plt

#colors kwarg let you define the sequence of the colors, 
#labels kwarg will assign a list of strings 
#containing the labels to be displayed in sequence
#axis() function to draw the pie chart in a perfectly spherical way

#a simple pie chart

labels = ['Nokia','Samsung','Apple','Lumia']
values = [10,30,45,15]
colors = ['yellow','green','red','blue']
plt.pie(values,labels=labels,colors=colors)
plt.axis('equal')
plt.show()


# explode kwarg let you draw a slice extracted from the pie
#add a title
#startangle kwarg to adjust the angle of rotation of the pie


import matplotlib.pyplot as plt

labels = ['Nokia','Samsung','Apple','Lumia']
values = [10,30,45,15]
colors = ['yellow','green','red','blue']
explode = [0.3,0,0,0]
plt.title('A Pie Chart')
plt.pie(values,labels=labels,colors=colors,explode=explode,startangle=180)
plt.axis('equal')
plt.show()


# autopct kwarg adds to the center of each slice a text label 
#showing the corresponding value.

import matplotlib.pyplot as plt

labels = ['Nokia','Samsung','Apple','Lumia']
values = [10,30,45,15]
colors = ['yellow','green','red','blue']
explode = [0.3,0,0,0]
plt.title('A Pie Chart')
plt.pie(values,labels=labels,colors=colors,explode=explode,shadow=True,autopct='%1.1f%%',startangle=180)
plt.axis('equal')
plt.show()


# ### Pie Charts with pandas DataFrame

import matplotlib.pyplot as plt
import pandas as pd

data = {'series1': [1,3,4,3,5],
        'series2': [2,4,5,2,4],
        'series3': [3,2,3,1,3]}
df = pd.DataFrame(data)
df['series1'].plot(kind='pie', figsize=(6,6))
plt.show()

















### Lecture 11. Google Map using gmplot package

import gmplot


import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)      #setting new working directory

# First two arugments are the geogrphical coordinates .i.e. Latitude and Longitude
#and the zoom resolution.

#Molde, Norway
#Note − Above screen display we see this because Google Maps 
#service is not free now in case you are accessing through an API.
# You need to add your API_KEY to see a better google map view. 
# Below is the code to accomplish this −
#gmap.apikey = "AIzaSyDeRNMnZ__VnQDiATiuz4kPjF_c9r1kWe8"

gmap1 = gmplot.GoogleMapPlotter(62.7426017,7.0121223, 18 )

#AALesund, Norway  
gmap2 = gmplot.GoogleMapPlotter(62.4681223,6.0766385,18)

#Oslo, Norway
gmap3 = gmplot.GoogleMapPlotter(59.8937879,10.4554652,18)

# Pass the path, to save
gmap1.draw( ".\\map11.html" )
gmap2.draw( ".\\map12.html" )
gmap3.draw( ".\\map13.html" )



#Scatter points on the google map and draw a line in between them .
# import gmplot package
import gmplot
  
latitude_list = [ 62.7426017, 62.4681223,59.8937879 ]
longitude_list = [7.0121223, 6.0766385,10.4554652 ]
  
gmap4 = gmplot.GoogleMapPlotter(59.8937879,10.4554652,13)
  
# scatter method of map object 
# scatter points on the google map
gmap4.scatter( latitude_list, longitude_list, '# FF0000',
                              size = 40, marker = False )
  
# Plot method Draw a line in
# between given coordinates
gmap4.plot(latitude_list, longitude_list,
            edge_width = 2.5)
  
gmap4.draw( ".\\map14.html" )



#To draw a polygon on the google map

# import gmplot package
import gmplot
  
latitude_list = [ 62.7426017, 62.4681223,59.8937879 ]
longitude_list = [7.0121223, 6.0766385,10.4554652 ]
  
gmap5 = gmplot.GoogleMapPlotter(59.8937879,10.4554652,13)
  
gmap5.scatter( latitude_list, longitude_list, '# FF0000',
                                size = 40, marker = False)
  
# polygon method Draw a polygon with
# the help of coordinates
gmap5.polygon(latitude_list, longitude_list,
                   color = 'cornflowerblue')
  
gmap5.draw( ".\\map15.html" )


















### Lecture 12. Google Map using folium package

#1 : To create a Base Map.
# import folium package
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path) 


# to install folium package, run the following in Anaconda prompt window:
# "pip install folium" 
import folium
 
# plot a base googlemap of Molde Football Stadium
# and starting Zoom level = 12
#find the coordinates from googlemap
my_map1 = folium.Map(location = [62.7330259, 7.1452503],
                                        zoom_start = 12 )
 
# save method of Map object will create a map
my_map1.save(" my_map1.html " ) 




#2 : Add a circular marker
import folium
 
my_map2 = folium.Map(location = [62.7330259,7.1452503],
                                         zoom_start = 12)
 
# CircleMarker with radius
folium.CircleMarker(location = [62.7330259,7.1452503],
                    radius = 50, popup = ' Molde Aker Stadium ').add_to(my_map2)
 
# save as html
my_map2.save(" my_map2.html ")



#3 : Add a simple_marker for parachute style marker with pop-up text. 
# import folium package
import folium
 
my_map3 = folium.Map(location = [62.7330259,7.1452503],
                                        zoom_start = 15)
 
# Pass a string in popup parameter
folium.Marker([62.7330259,7.1452503],
               popup = ' Molde Aker Stadium ').add_to(my_map3)
 
 
my_map3.save(" my_map3.html ")






#4 : Add a line to the map
import folium
 
my_map4 = folium.Map(location = [62.7330259,7.1452503],
                                        zoom_start = 12)
 
folium.Marker([62.7330259,7.1452503],
              popup = 'Molde Aker Stadium').add_to(my_map4)
 
folium.Marker([62.4701708,6.1854081],
              popup = 'AAlesund Colorline Stadium').add_to(my_map4)
 
# Add a line to the map by using line method .
# it connect both coordinates by the line
# line_opacity implies intensity of the line
 
folium.PolyLine(locations = [(62.7330259,7.1452503), (62.4701708,6.1854081)],
                line_opacity = 0.5).add_to(my_map4)
 
my_map4.save("my_map4.html")
















### Lecture 13. Donut charts

#Creating a Simple Donut Chart
#Create a Pie Chart
#Draw a circle of suitable dimensions.
#Add circle at the Center of Pie chart

import matplotlib.pyplot as plt
 
# Setting labels for items in Chart
Familymember = ['Wilson', 'Dudu', 'Maomao',
            'Miaomiao', 'Mico', 'Mia']
 
# Setting size in Chart based on
# given values
Income = [32000, 20000, 22000, 10000, 8000, 3000]
 
# colors
colors = ['#FF0000', '#0000FF', '#FFFF00',
          '#ADFF2F', '#FFA500','#FF00FF',]
# explosion
explode = (0.05, 0.05, 0.05, 0.05, 0.05, 0.05)


# Pie Chart
plt.pie(Income, colors=colors, labels=Familymember,
        autopct='%1.1f%%', pctdistance=0.85,
        explode=explode)
 
# draw circle
centre_circle = plt.Circle((0, 0), 0.70, fc='white')
#gcf() : get current figure
fig = plt.gcf()
 
# Adding Circle in Pie chart
#gca() : get current axes
fig.gca().add_artist(centre_circle)
 
# Adding Title of chart
plt.title('Employee Salary Details')
 
# Displaying Chart
plt.show()




#Customizing the Donut Chart
#Adding Legends to the Donut Chart 

import matplotlib.pyplot as plt
 
 
# Setting size in Chart based on
# given values
Income = [32000, 20000, 22000, 10000, 8000, 3000]
 
# Setting labels for items in Chart
Familymember = ['Wilson', 'Dudu', 'Maomao',
            'Miaomiao', 'Mico', 'Mia']
 
# colors
colors = ['#FF0000', '#0000FF', '#FFFF00',
          '#ADFF2F', '#FFA500','#FF00FF',]
 
# explosion
explode = (0.05, 0.05, 0.05, 0.05, 0.05,0.05)


# Pie Chart
plt.pie(Income, colors=colors, labels=Familymember,
        autopct='%1.1f%%', pctdistance=0.85,
        explode=explode)
 
# draw circle
centre_circle = plt.Circle((0, 0), 0.70, fc='white')
fig = plt.gcf()        #get the current figure
 
# Adding Circle in Pie chart
#gca() to get the current axes
fig.gca().add_artist(centre_circle)


# Adding Title of chart
plt.title('Family Member Income')
 
# Add Legends
plt.legend(Familymember, loc="upper right", title="Member Color")
 
# Displaying Chart
plt.show()






















### Lecture 14. Stack plot

#example 1. Family income
import matplotlib.pyplot as plt
 
# List of family member
Familymember = ['Wilson', 'Dudu', 'Maomao', 'Miaomiao', 'Mico', 'Mia']
 
# Income list
Salary = [7, 8, 6, 11, 7,5]
 
# Allowance list
Allowance =  [8, 5, 7, 8, 13,4]
 
# Stackplot with X, Y, colors value
plt.stackplot(Familymember, Salary, Allowance,
              colors =['r', 'c'])
 
# X label
plt.xlabel('Familymember')
 
# Y label
plt.ylabel('Income')
 
# Title of Graph
plt.title('Income of Salary and Allowance')
 
# Displaying Graph
plt.show()


## example 2. patients cases
import matplotlib.pyplot as plt
 
# List of 7-days
days = [x for x in range(0, 7)]
 
# List of Suspected cases
Suspected = [12, 18, 35, 50, 72, 90, 100]
 
# List of Cured Cases
Cured = [4, 8, 15, 22, 41, 55, 62]
 
# List of Number of deaths
Deaths = [1, 3, 5, 7, 9, 11, 13]
 
# Plot x-labels, y-label and data
plt.plot([], [], color ='blue',
         label ='Suspected')
plt.plot([], [], color ='orange',
         label ='Cured')
plt.plot([], [], color ='brown',
         label ='Deaths')
 
# Implementing stackplot on data
plt.stackplot(days, Suspected, Cured,
              Deaths, baseline ='zero',
              colors =['blue', 'orange',
                       'brown'])
 
plt.legend()
 
plt.title('No of Cases')
plt.xlabel('Day of the week')
plt.ylabel('Overall cases')
 
plt.show()




## example 3. Patient cases, value of baseline is set to sym 
import matplotlib.pyplot as plt
 
# List of 7-days
days = [x for x in range(0, 7)]
 
# List of Suspected cases
Suspected = [12, 18, 35, 50, 72, 90, 100]
 
# List of Cured Cases
Cured = [4, 8, 15, 22, 41, 55, 62]
 
# List of Number of deaths
Deaths = [1, 3, 5, 7, 9, 11, 13]
 
# Plot x-labels, y-label and data
plt.plot([], [], color ='blue',
         label ='Suspected')
plt.plot([], [], color ='orange',
         label ='Cured')
plt.plot([], [], color ='brown',
         label ='Deaths')
 
# Implementing stackplot on data
plt.stackplot(days, Suspected, Cured,
              Deaths, baseline ='sym',
              colors =['blue', 'orange',
                       'brown'])
 
plt.legend()
 
plt.title('No of Cases')
plt.xlabel('Day of the week')
plt.ylabel('Overall cases')
 
plt.show()














### Lecture 15. Box plot

# Import libraries
import matplotlib.pyplot as plt
import numpy as np
 
# example a simple boxplot for 1000 random numbers with
#normal distribution
# Creating dataset
np.random.seed(10)
data = np.random.normal(50, 10,1000 )
 
fig = plt.figure(figsize =(8, 6))
 
# Creating plot
plt.boxplot(data)
 
# show plot
plt.show()



#Customizing Box Plot
#The notch = True attribute creates the notch format to the box plot,
#patch_artist = True fills the boxplot with colors, 
#we can set different colors to different boxes.
#The vert = 0 attribute creates horizontal box plot. 
#labels takes same dimensions as the number data sets.

# Example 2 plot 4 boxplots in one plot
# Import libraries
import matplotlib.pyplot as plt
import numpy as np
 
 
# Creating dataset
np.random.seed(10)
 
data_1 = np.random.normal(80, 5, 1000)
data_2 = np.random.normal(70, 10, 1000)
data_3 = np.random.normal(60, 15, 1000)
data_4 = np.random.normal(50, 20, 1000)
data = [data_1, data_2, data_3, data_4]
 
fig = plt.figure(figsize =(10, 7))
 
# Creating axes instance
#denoting the lower left point of the new axes in figure coodinates
# (0,0) and its width and height(1,1). 
ax = fig.add_axes([0, 0, 1, 1])
 
# Creating plot
bp = ax.boxplot(data)
 
# show plot
plt.show()






## example 3. customizing each box in plot

# Import libraries
import matplotlib.pyplot as plt
import numpy as np
 
# Creating dataset
np.random.seed(10)
data_1 = np.random.normal(100, 10, 200)
data_2 = np.random.normal(90, 20, 200)
data_3 = np.random.normal(80, 30, 200)
data_4 = np.random.normal(70, 40, 200)
data = [data_1, data_2, data_3, data_4]
 
fig = plt.figure(figsize =(10, 7))
ax = fig.add_subplot(111)
 
# Creating axes instance
bp = ax.boxplot(data, patch_artist = True,
                notch ='True', vert = 0)
 
colors = ['#0000FF', '#00FF00',
          '#FFFF00', '#FF00FF']
 
#zip will create a list of tuple of (box, color)
#then set color for each box
for patch, color in zip(bp['boxes'], colors):
    patch.set_facecolor(color)
 
# changing color and linewidth of
# whiskers
for whisker in bp['whiskers']:
    whisker.set(color ='#8B008B',
                linewidth = 1.5,
                linestyle =":")
 
# changing color and linewidth of caps
# (boundary for outlier)
for cap in bp['caps']:
    cap.set(color ='#8B008B',
            linewidth = 2)
 
# changing color and linewidth of
# medians
for median in bp['medians']:
    median.set(color ='red',
               linewidth = 3)
 
# changing style of fliers
for flier in bp['fliers']:
    flier.set(marker ='D',
              color ='#e7298a',
              alpha = 0.5)
     
# x-axis labels
ax.set_yticklabels(['data_1', 'data_2',
                    'data_3', 'data_4'])
 
# Adding title
plt.title("Customized box plot")
 
# Removing top axes and right axes
# ticks
ax.get_xaxis().tick_bottom()
ax.get_yaxis().tick_left()
     
# show plot
plt.show()

















### Lecture 16. Plot Subplots Within Other Subplots

import matplotlib.pyplot as plt


# ## Multi-Panel Plots

# ### Display Subplots within Other Subplots


import matplotlib.pyplot as plt

fig = plt.figure()
ax = fig.add_axes([0.1,0.1,0.8,0.8])
inner_ax = fig.add_axes([0.6,0.6,0.25,0.25])

plt.show()



import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_axes([0.1,0.1,0.8,0.8])
inner_ax = fig.add_axes([0.6,0.6,0.25,0.25])
x1 = np.arange(10)
y1 = np.array([1,2,7,1,5,2,4,2,3,1])
x2 = np.arange(10)
y2 = np.array([1,3,4,5,4,5,2,6,4,3])
ax.plot(x1,y1)
inner_ax.plot(x2,y2)
plt.show()
















### Lecture 17. Drawing 2D Heatmap using Matplotlib in Python
#Basic Heatmap Using Python Matplotlib Library 
#Create a 10×10 Heatmap with Random data using Matplotlib

# Program to plot 2-D Heat map
# using matplotlib.pyplot.imshow() method
import numpy as np
import matplotlib.pyplot as plt
 
data = np.random.random(( 10 , 10 ))
plt.imshow( data )
 
plt.title( "2-D Heat Map" )
plt.show()



#Choosing Different Colormaps in Heatmap Using Matplotlib
# using the cmap parameter

# Program to plot 2-D Heat map
# using matplotlib.pyplot.imshow() method
import numpy as np
import matplotlib.pyplot as plt
 
data = np.random.random((10, 10))
plt.imshow(data, cmap='autumn')
 
plt.title("Heatmap with different color")
plt.show()


#Adding Colorbar to Heatmap Using Matplotlib to show the 
# weight of color relatively between a certain range.
# using plt.colorbar().

data = np.random.random((12, 12))
plt.imshow(data, cmap='autumn', interpolation='nearest')
 
# Add colorbar
plt.colorbar()
 
plt.title("Heatmap with color bar")
plt.show()



#Customized Heatmap 
#use plt.annotate()  to annotate values in the heatmap

import matplotlib.colors as colors
 
# Generate random data
data = np.random.randint(0, 100, size=(8, 8))
 
# Create a custom color map
# with blue and green colors
colors_list = ['#0099ff', '#33cc33']
cmap = colors.ListedColormap(colors_list)
 
# Plot the heatmap with custom colors and annotations
#vmin and vmax define the data range that the colormap covers. 
#extent: The bounding box in data coordinates that the image will fill.
plt.imshow(data, cmap=cmap, vmin=0, vmax=100, extent=[0, 8, 0, 8])
for i in range(8):
    for j in range(8):
        plt.annotate(str(data[i][j]), xy=(j+0.5, i+0.5),
                     ha='center', va='center', color='white')
 
# Add colorbar
cbar = plt.colorbar(ticks=[0, 50, 100])
cbar.ax.set_yticklabels(['Low', 'Medium', 'High'])
 
# Set plot title and axis labels
plt.title("Customized heatmap with annotations")
plt.xlabel("X-axis")
plt.ylabel("Y-axis")
 
# Display the plot
plt.show()




# Heatmap Using Seaborn Library

# importing the modules
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
   
# generating 2-D 10x10 matrix of random numbers
# from 1 to 100
data = np.random.randint(low=1,
                         high=100,
                         size=(10, 10))
   
# plotting the heatmap
hm = sns.heatmap(data=data,
                annot=True)
   
# displaying the plotted heatmap
plt.show()































# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""



### Section 16. Plotting with Pandas and Seaborn

## Lecture 1. Line Plots and Bar Plots with Pandas

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib

#line plot of a Series
s = pd.Series(np.random.randn(10).cumsum(), index=np.arange(0, 100, 10))
s.plot(title='Cumulative Random Number')



#Line plot of a DataFrame


data = {'length' : [28, 56, 32, 77, 62,45, 21, 43 ],
        'weight' : [150, 221, 123, 173, 293, 98, 78, 108],
        'price' : [1.2, 1.0, 0.6, 0.9, 1.7,1.4,0.9,1.8]}
frame = pd.DataFrame(data)
frame
frame.plot(figsize = (10,6), title = 'Product Information')

#Line plot of one variable of a DataFrame
frame.plot(y='price',figsize = (10,6), title = 'Product Information', ylabel='USD')


# Bar Plots of 2 Series in each subplot

fig, axes = plt.subplots(2, 1)
data = pd.Series(np.random.rand(16), index=list('abcdefghijklmnop'))
data.plot.bar(ax=axes[0], color='k', alpha=0.7)
data.plot.barh(ax=axes[1], color='k', alpha=0.7)





np.random.seed(12348)

#bar plot of DataFrame

df = pd.DataFrame(np.random.rand(6, 4),
                  index=['one', 'two', 'three', 'four', 'five', 'six'],
                  columns=pd.Index(['A', 'B', 'C', 'D'], name='Genus'))
df
df.plot.bar(title='Product Info')


plt.figure()



#Horizontal stacked barplot of DataFrame

df.plot.barh(stacked=True, alpha=0.5, title='Product Info')



plt.close('all')




# Bar plot, Normalizing to sum to 1 each row

df = pd.DataFrame(np.random.rand(6, 4),
                  index=['one', 'two', 'three', 'four', 'five', 'six'],
                  columns=pd.Index(['A', 'B', 'C', 'D'], name='Genus'))
df

df_norm = df.div(df.sum(1), axis=0)
df_norm
df_norm.plot.bar(title='Product Info(in percentage)')




plt.close('all')

















### Lecture 2. Swarm plot using Seaborn

# Example 1: Basic visualization of “fmri” dataset using swarmplot() 

import seaborn
 
 
seaborn.set(style='whitegrid')
fmri = seaborn.load_dataset("fmri")
 
seaborn.swarmplot(x="timepoint",
                  y="signal",
                  data=fmri,
                  palette="Set2",
                  s=1.5)



#Example 2: Grouping data points on the basis of category, 
#here as region and event.

import seaborn
 
 
seaborn.set(style='whitegrid')
fmri = seaborn.load_dataset("fmri")
 
seaborn.swarmplot(x="timepoint",
                  y="signal",
                  hue="region",
                  data=fmri,
                  palette="Set2",
                  s=5)



#Example 3. students scores by gender and country
import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)

dataset = pd.read_csv("./University-Fullname-3column.csv")

dataset.head()

seaborn.swarmplot( x ='Gender', y= 'Math', hue='Country', data=dataset,
         palette="Set2", dodge=True, s=10)















### Lecture 3. Joint plot using Seaborn

#seaborn.jointplot(x,  y,  data, stat_func)

#example 1, a basic Joint plot
import seaborn as sns
import matplotlib.pyplot as plt


import numpy as np
import pandas as pd
#setting working directory
import os
work_path="d:\\PythonBeginningCourse"
os.chdir(work_path)

dataset = pd.read_csv("./University-Fullname-full.csv")

dataset.head()

list(dataset.columns)


sns.jointplot(x = "Math", y = "Physics",
               data = dataset)
# show the plot
plt.show()


#example 2. add hue for grouping
sns.jointplot(x = "Math", y = "Physics",
               data = dataset, hue='Gender')
# show the plot
plt.show()


#example 3: add regression line
sns.jointplot(x = "Math", y = "Physics",
               data = dataset, kind='reg')
# show the plot
plt.show()

  


## example 4. Add hex kind
sns.jointplot(x = "Math", y = "Physics",
               data = dataset, kind='hex')
# show the plot
plt.show()



## example 5. Add kde kind
sns.jointplot(x = "Math", y = "Physics",
               data = dataset, kind='kde')
# show the plot
plt.show()






















### Lecture 2. Seaborn plotting tutorial

#Getting Started
#a simple line plot using the iris dataset
import seaborn as sns 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data)


#Using Seaborn with Matplotlib
#invoke the Seaborn Plotting function as normal, and then we 
#can use Matplotlib’s customization function.

#using the above example and will add the title to the plot 
#using the Matplotlib.

# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# setting the title using Matplotlib
plt.title('Title using Matplotlib and Seaborn')
  
plt.show()


#Setting the xlim and ylim
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# setting the x limit of the plot
plt.xlim(5)
  
plt.show()




#Customizing Seaborn Plots
#Changing Figure Aesthetic
#There are five themes available in Seaborn.
#darkgrid
#whitegrid
#dark
#white
#ticks

#Using the dark theme

# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# changing the theme to dark
sns.set_style("dark")
plt.show()



#Removal of Spines
#Spines are the lines noting the data boundaries and connecting 
#the axis tick marks. It can be removed using the despine() method.
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# Removing the spines
sns.despine()
plt.show()




#Changing the figure Size
#The figure size can be changed using the figure() method of Matplotlib
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# changing the figure size
plt.figure(figsize = (2, 4))
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# Removing the spines
sns.despine()
  
plt.show()


#Scaling the plots
#It can be done using the set_context() method. It allows us to 
#override default parameters. This affects things like the 
#size of the labels, lines, and other elements of the plot, 
#but not the overall style. The base context is “notebook”, 
#and the other contexts are “paper”, “talk”, and “poster”. 
#font_scale sets the font size.

# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
# draw lineplot 
sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
# Setting the scale of the plot
sns.set_context("paper")
  
plt.show()



#Setting the Style Temporarily
#axes_style() method is used to set the style temporarily. 
#It is used along with the with statement.
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt 
  
# loading dataset 
data = sns.load_dataset("iris") 
  
  
def plot():
    sns.lineplot(x="sepal_length", y="sepal_width", data=data) 
  
with sns.axes_style('darkgrid'):
      
    # Adding the subplot
    plt.subplot(211)
    plot()
      
plt.subplot(212)
plot()



#Color Palette
#Colormaps are used to visualize plots effectively and easily. 
#One might use different sorts of colormaps for different kinds 
#of plots. color_palette() method is used to give colors to the plot. 
#Another function palplot() is used to deal with the color palettes 
#and plots the color palette as a horizontal array.


# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt
  
# current colot palette
palette = sns.color_palette()
  
# plots the color palette as a
# horizontal array
sns.palplot(palette)
  
plt.show()



#Diverging Color Palette
#This type of color palette uses two different colors where 
#each color depicts different points ranging from a common 
#point in either direction. Consider a range of -10 to 10 so the 
#value from -10 to 0 takes one color and values from 0 to 10 take 
#another.

#In the following example, we use an in-built diverging color 
#palette which shows 11 different points of color. The color 
#on the left shows pink color and color on the right shows 
#green color.

# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt
  
# current colot palette
palette = sns.color_palette('PiYG', 11)
  
# diverging color palette
sns.palplot(palette)
  
plt.show()





#Sequential Color Palette
#A sequential palette is used where the distribution ranges from 
#a lower value to a higher value. To do this add the character 
#‘s’ to the color passed in the color palette.
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt
  
# current colot palette
palette = sns.color_palette('Greens', 11)
  
# sequential color palette
sns.palplot(palette)
  
plt.show()




#Setting the default Color Palette
#set_palette() method is used to set the default color palette 
#for all the plots. The arguments for both color_palette() and 
#set_palette() is same. set_palette() changes the default 
#matplotlib parameters.

# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt
  
# loading dataset 
data = sns.load_dataset("iris") 
  
def plot():
    sns.lineplot(x="sepal_length", y="sepal_width", data=data)
  
# setting the default color palette
sns.set_palette('vlag')
plt.subplot(211)
  
# plotting with the color palette
# as vlag
plot()
  
# setting another default color palette
sns.set_palette('Accent')
plt.subplot(212)
plot()
  
plt.show()



#Multiple plots with Seaborn

#Using Matplotlib

#Using add_axes() method
# importing packages
import seaborn as sns
import matplotlib.pyplot as plt
  
  
# loading dataset
data = sns.load_dataset("iris")
  
def graph():
    sns.lineplot(x="sepal_length", y="sepal_width", data=data)
  
# Creating a new figure with width = 5 inches
# and height = 4 inches
fig = plt.figure(figsize =(5, 4)) 
  
# Creating first axes for the figure
ax1 = fig.add_axes([0.1, 0.1, 0.8, 0.8]) 
  
# plotting the graph
graph()
  
# Creating second axes for the figure
ax2 = fig.add_axes([0.5, 0.5, 0.3, 0.3])
  
# plotting the graph
graph()
  
plt.show()
 



#Using subplot() method
# importing packages 
import seaborn as sns 
import matplotlib.pyplot as plt
  
# loading dataset 
data = sns.load_dataset("iris") 
  
def graph():
    sns.lineplot(x="sepal_length", y="sepal_width", data=data)
  
# Adding the subplot at the specified
# grid position
plt.subplot(121)
graph()
  
# Adding the subplot at the specified
# grid position
plt.subplot(122)
graph()
  
plt.show()
 
 

#Using subplot2grid() method
# importing packages
import seaborn as sns
import matplotlib.pyplot as plt
  
# loading dataset
data = sns.load_dataset("iris")
  
def graph():
    sns.lineplot(x="sepal_length", y="sepal_width", data=data)
  
# adding the subplots
axes1 = plt.subplot2grid (
  (7, 1), (0, 0), rowspan = 2,  colspan = 1) 
graph()
  
axes2 = plt.subplot2grid (
  (7, 1), (2, 0), rowspan = 2, colspan = 1) 
graph()
    
axes3 = plt.subplot2grid (
  (7, 1), (4, 0), rowspan = 2, colspan = 1)
graph()





#Using Seaborn 

#Using FacetGrid() method

#FacetGrid class helps in visualizing distribution of one variable
# as well as the relationship between multiple variables separately
# within subsets of your dataset using multiple panels.
#A FacetGrid can be drawn with up to three dimensions ? row, col, 
#and hue. The first two have obvious correspondence with the 
#resulting array of axes; think of the hue variable as a third 
#dimension along a depth axis, where different levels are plotted 
#with different colors.
#FacetGrid object takes a dataframe as input and the names of 
#the variables that will form the row, column, or hue dimensions 
#of the grid. The variables should be categorical and the data at 
#each level of the variable will be used for a facet along that axis.

# importing packages
import seaborn as sns
import matplotlib.pyplot as plt
  
# loading dataset
data = sns.load_dataset("iris")
  
plot = sns.FacetGrid(data, col="species")
plot.map(plt.plot, "sepal_width")
  
plt.show()
 


#Using PairGrid() method
#Subplot grid for plotting pairwise relationships in a dataset.
#This class maps each variable in a dataset onto a column and 
#row in a grid of multiple axes. Different axes-level plotting 
#functions can be used to draw bivariate plots in the upper and 
#lower triangles, and the marginal distribution of each variable 
#can be shown on the diagonal.
#It can also represent an additional level of conventionalization 
#with the hue parameter, which plots different subsets of data 
#in different colors. This uses color to resolve elements on a 
#third dimension, but only draws subsets on top of each other 
#and will not tailor the hue parameter for the specific visualization
#the way that axes-level functions that accept hue will.


# importing packages
import seaborn as sns
import matplotlib.pyplot as plt
  
# loading dataset
data = sns.load_dataset("flights")
  
plot = sns.PairGrid(data)
plot.map(plt.plot)
  
plt.show()





#Creating Different Types of Plots






































































# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
How to do data analysis with Python programming

@author: https://www.youtube.com/@easydatascience2508
"""




### Section 17. Python data processing examples


## Example 1. Concatenate Multiple Text Files in to one object
import os
os.getcwd()    #gettinh current working directory
#Setting new working directory
work_path="d:\\Pythondemo"
os.chdir(work_path)      #setting new working directory
os.getcwd()


import glob

# Load all txt files in path
demofiles = glob.glob('.\\*.txt')
demofiles

# Concatenate files to new file
with open('demo_output.txt', 'w') as out_file:
    for file in demofiles:
        with open(file) as from_file:
            out_file.write(from_file.read())
            out_file.write("\n")

# Read file and print
with open('demo_output.txt', 'r') as new_file:
    lines = [line.strip() for line in new_file]
for line in lines: print(line)



















#Example 2. Check if a Substring is Present in a Given String


#Method 1: using the if… in

lstring = "I like both programming language Python and R"
 
if "like" in lstring:
    print("The string is present")
else:
    print("The is not present")


dir(lstring)


#Method 2: using the split() method
#First split the given string into words and store them in a variable
#then using the if condition

lstring = "I like both programming language Python and R"
sstring = "Python"  
 
# splitting words in a given string
s = lstring.split()
 
# checking condition

if sstring in s:
    print("yes")
else:
    print("no")




#Method3: using the find() method
#inbuilt function find() which checks if a substring is present 
#in the string, which is done in one line. 
#find() function returns -1 if it is not found, 
#else it returns the first occurrence

def check(string, sub_str):
    if (string.find(sub_str) == -1):
        print("NO")
    else:
        print("YES")
 
 
# driver code
lstring = "I like both programming language Python and R"
sstring = "Python" 
check(lstring, sstring)




#Method 4: using “count()” method
#count the number of occurrences of substring in a string
#If the substring is found then “yes ” will print otherwise “no will be printed”.

def check(string, substring):
    if (string.count(substring) > 0):
        print("YES")
    else:
        print("NO")
 
 
lstring = "I like both programming language Python and R"
sstring = "Python" 
check(lstring, sstring)




#Method 5: using regular expressions 
#RegEx can be used to check if a string contains the specified 
#search pattern. Python has a built-in package called re, 
#which can be used to work with Regular Expressions.

import re
 
lstring = "I like both programming language Python and R"
sstring = "Python" 
 
# re.search() returns a Match object
# if there is a match anywhere in the string
if re.search(sstring, lstring):
    print("YES,string '{0}' is present in string '{1}'" .format(
        sstring, lstring))
else:
    print("NO,string '{0}' is not present in string '{1}' " .format(
        sstring, lstring))












### Example 3. Concatenate Multiple CSV Files Into a DataFrame

#We first get a list of the CSV files in our path; then, 
#for each file in the path, we read the contents into its 
#own dataframe; afterwards, we combine all dataframes into 
#a single frame; finally, we print out the results to inspect.

import os
os.getcwd()    #gettinh current working directory
#Setting new working directory
work_path="d:\\Pythondemo"
os.chdir(work_path)      #setting new working directory
os.getcwd()


import glob
import pandas as pd


# Load all txt files in path
demofiles = glob.glob('.\\*.csv')
demofiles      

# Create a list of dataframe, one series per CSV
full_list = []
for file_name in demofiles:
    df = pd.read_csv(file_name, index_col=None, header=None)
    full_list.append(df)

# Create combined frame out of list of individual frames
full_frame = pd.concat(full_list, axis=0, ignore_index=True)

print(full_frame)






 








### Example 4. Zip & Unzip Files with Pandas
import os
os.getcwd()    #gettinh current working directory
#Setting new working directory
work_path="d:\\Pythondemo"
os.chdir(work_path)      #setting new working directory
os.getcwd()
import pandas as pd

# Create a dataframe
df = pd.DataFrame({'object': ['ball', 'pen', 'paper'],
	           'price': ['10', '5', '2'],
		   'color': ['blue', 'green', 'white']})

# Compress and save dataframe to file
df.to_csv('savedfile.csv.zip', index=False, compression='zip')
print('Dataframe compressed and saved to file')

# Read compressed zip file into dataframe
df_unzipped = pd.read_csv('savedfile.csv.zip',)
print(df_unzipped)
















### Example 5. Merge two lists into a dictionary

listA = ['Wilson', 'Dudu', 'Mico', 'Miaomiao', 'Wilson']
listB = [32, 20, 7, 10, 69]

#There are 3 ways to convert these two lists into a dictionary

#1- Using Python's zip, dict functionz
dict_1 = dict(zip(listA, listB))

dict_1

#2- Using the zip function with dictionary comprehensions
dict_2 = {key:value for key, value in zip(listA, listB)}

dict_2

#3- Using the zip function with a loop
items_tuples = zip(listA, listB) 
dict_3 = {} 
for key, value in items_tuples: 
    if key in dict_3: 
        pass # To avoid repeating keys.
    else: 
        dict_3[key] = value

dict_3















### Example 6. Merge two or more lists into a list of lists

#task is when we have two or more lists, and we want to collect 
#them all in one big list of lists, where all the first element (list)
#in the resulting list come from all the first elements from input
#lists, and so on. 

#For example, if I have 4 lists [1,2,3], [‘a’,’b’,’c’], [‘h’,’e’,’y’] 
#and [4,5,6] ,and we want to make a new list of those four lists; 
#it will be [[1,’a’,’h’,4], [2,’b’,’e’,5], [3,’c’,’y’,6]].



def merge(*args, missing_val = None):
    """missing_val will be used when one of the smaller lists is 
       shorter than the others.
       Get the maximum length within the smaller lists."""
    
    max_length = max([len(lst) for lst in args])
    outList = []
    for i in range(max_length):
        result = []
        for k in range(len(args)):
            if i < len(args[k]):
                result.append(args[k][i])
            else:
                result.append(missing_val)
        outList.append(result)
 
    return outList


testlistA = ["Wilson", "Dudu", "Maomao", "Miaomiao", "Mico", "Mia"]
testlistB = [32, 20, 22, 10, 7]

testlistC = merge(testlistA, testlistB)

testlistC



#Alternative code, more concise, for more experienced people
def merge(*args, missing_val = None):
    """ missing_val will be used when one of the smaller lists is shorter tham the others.
        Get the maximum length within the smaller lists."""
    max_length = max([len(lst) for lst in args])
    outList = []
    for i in range(max_length):
        result = []
        result.append([args[k][i] if i < len(args[k]) else missing_val for k in range(len(args))])
        outList.append(result)
    
    return outList



# use zip, result is list of tupoes, and length is the length of 
#the shortest input list
list(zip(testlistA, testlistB))












### Example 7. Sort a list of dictionaries

dicts_lists = [
  {
    "Name": "Wilson",
    "Age": 32,
  },
  {
     "Name": "Dudu",
     "Age": 20,
  },
  {
    "Name": "Maomao",
    "Age": 22,
  }
]

#There are different ways to sort that list
#1- Using the sort/ sorted function based on the age
#using dictionary's get method
dicts_lists.sort(key=lambda item: item.get("Age"))

dicts_lists

#2- Using itemgetter module based on name
from operator import itemgetter

dicts_lists = [
  {
    "Name": "Wilson",
    "Age": 32,
  },
  {
     "Name": "Dudu",
     "Age": 20,
  },
  {
    "Name": "Maomao",
    "Age": 22,
  }
]

sortkey = itemgetter('Name')
dicts_lists.sort(key=sortkey)

dicts_lists



















### Example 8. Sort a list based on another list

a = ['Wilson', 'Dudu', 'Maomao', 'Mico', 'MiaoMiao', 'Mia']
b = [32, 12, 25, 41, 18,69]

#simplest method
[x for _, x in sorted(zip(b, a))]

#Use list comprehensions to sort these lists
#zip the two lists.
#create a new, sorted list based on the zip using sorted().
#using a list comprehension extract the first elements of each pair from the sorted, zipped list.

sortedList =  [val for (_, val) in sorted(zip(b, a), key=lambda x: \
          x[0])]


sortedList














### Example 9. Removing unwanted characters from string

#Removing symbol from string using str.isalnum()

#isalnum() method checks whether all the characters in a 
#given string are alphanumeric or not

string = "Wi;lson * an:d ! Mi;co1 * a*n:d ! Mi:ao2:"
 
test_str = ''.join(letter for letter in string if letter.isalnum())
print(test_str)


#Removing symbol from string using replace() 
#str.replace() inside a loop can check for a bad_char and 
#then replace it with the empty string hence removing it. 

# initializing bad_chars_list
remove_chars = [';', ':', '!', "*", " "]
 
# initializing test string
test_string = "Wil;son * a:nd ! Mi;co * and*Mi:ao !"
 
# printing original string
print("Original String : " + test_string)
 
# using replace() to
# remove bad_chars
for i in remove_chars:
    test_string = test_string.replace(i, '')
 
# printing resultant string
print("Resultant list is : " + str(test_string))


def remove_char(inputstring, badstring):
    """Remove unwanted string from inputstring"""
    for i in badstring:
        inputstring = inputstring.replace(i, '')
    return inputstring

remove_char(test_string,remove_chars)


#Removing symbol from string using join() + generator

#By using Python join() we remake the string. 
#In the generator function, we specify the logic to ignore 
#the characters in bad_chars and hence construct a new 
#string free from bad characters.

# initializing bad_chars_list
bad_chars = [';', ':', '!', "*", " "]
 
# initializing test string
test_string = "Wil;son * a:nd ! Mi;co * Mi*ao:miao !"
 
# printing original string
print("Original String : " + test_string)
 
# using join() + generator to
# remove bad_chars
test_string = ''.join(i for i in test_string if not i in bad_chars)
 
# printing resultant string
print("Resultant list is : " + str(test_string))


































# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 12:45:46 2022
Doing Statistics using Python 

@author: https://www.youtube.com/@easydatascience2508
"""

### Lecture 1. Mean, Median, Mode


## list
L1 = [10,8,6,4,5,3,7,2,9,1,8,4,34,32,4,9,8,6,0,3,7,8]
def Average(lst):
    return sum(lst) / len(lst)

ave = Average(L1)      #mean
ave


L1.sort()
mid = len(L1) // 2
res = (L1[mid] + L1[-mid-1]) / 2      #median
res
 

from statistics import mode
mode(L1)      #mode





##numpy ndarray
import numpy as np

ages = np.random.randint(18, high=90, size=500)
np.mean(ages)      #mean

np.median(ages)    #median

from scipy import stats
stats.mode(ages)      #mode





## pandas series

import pandas as pd
import numpy as np

s = pd.Series([12, -4, 7, 9, 9], index=['a', 'b', 'c', 'd', 'e'])
s
  
s.mean()     #mean
s.median()   #median
s.mode()







## pandas DataFrame

df = pd.DataFrame({"A":[12, 5, 5, 44, 1],
                "B":[5, 2, 54, 3, 2],
                "C":[20, 16, 7, 16, 8],
                "D":[14, 2, 17, 2, 6]})
  
df

#mean of each column 
df.mean(axis = 0)

#mean of each row
df.mean(axis = 1)

#mean of specific column
df['A'].mean()

#mean of a specific row
df.loc[1].mean()

#median of each column 
df.median(axis = 0)

#median of each row
df.median(axis = 1)


#median of specific column
df['A'].median()

#median of a specific row
df.loc[1].median()

#mode of each column
df.mode(axis=0)

#mode of each row
df.mode(axis=1)

#mode of specific column
df['A'].mode()

#mean of a specific row
df.loc[1].mode()

















### Lecture 2. Binomial distribution

#The scipy.stats module contains various functions for statistical 
#calculations and tests. The stats() function of the 
#scipy.stats.binom module can be used to calculate a binomial
# distribution using the values of n and p.

#Syntax : 
#scipy.stats.binom.stats(n, p)
#It returns a tuple containing the mean and variance of the 
# distribution in that order.

#example: Calculating distribution table :

#Define n and p.
#Define a list of values of r from 0 to n.
#Get mean and variance.
#For each r, calculate the pmf and store in a list.

from scipy.stats import binom

# setting the values
# of n and p
n = 10
p = 0.2
# defining the list of r values
r_values = list(range(n + 1))
# obtaining the mean and variance 
mean, var = binom.stats(n, p)

mean
var


#scipy.stats.binom.pmf() function is used to obtain the 
#probability mass function for a certain value of r, n and p. 
#We can obtain the distribution by passing all possible values 
#of r(0 to n).

#Syntax : 
# scipy.stats.binom.pmf(r, n, p)

# list of pmf values
dist = [binom.pmf(r, n, p) for r in r_values ]
# printing the table
print("r\tp(r)")
for i in range(n + 1):
    print(str(r_values[i]) + "\t" + str(dist[i]))
# printing mean and variance
print("mean = "+str(mean))
print("variance = "+str(var))



# example: Plotting the graph using matplotlib.pyplot.bar() 
#function to plot vertical bars.

from scipy.stats import binom
import matplotlib.pyplot as plt
# setting the values
# of n and p
n = 1000
p = 0.2
# defining list of r values
r_values = list(range(n + 1))
# list of pmf values
dist = [binom.pmf(r, n, p) for r in r_values ]
# plotting the graph 
plt.bar(r_values, dist)
plt.show()














### Lecture 3. Poisson distribution

#How to Generate a Poisson Distribution
#You can use the poisson.rvs(mu, size) function to generate 
#random values from a Poisson distribution with a specific 
#mean value and sample size:

    
from scipy.stats import poisson

#generate random values from Poisson distribution with mean=8 and sample size=20
poisson.rvs(mu=8, size=20)


# How to Calculate Probabilities Using a Poisson Distribution
# You can use the poisson.pmf(k, mu) 
#to calculate probabilities related to the specific count value
#from Poisson distribution.

#Example 1: Probability Equal to Some Value

#A store sells 8 icecreams per day on average. What is the 
#probability that they will sell 10 icecreams on a given day? 

from scipy.stats import poisson

#calculate probability
poisson.pmf(k=10, mu=8)


#You can use the poisson.cdf(k, mu) functions to calculate 
#cumulative probabilities up to a certain discrete value
# the Poisson distribution.

#Example 2: Probability Less than Some Value
#A call center has on average 5 calls coming in per hour. 
# What is the probability that this call center has four or less incoming calls
# during a given hour?

from scipy.stats import poisson

#calculate probability
poisson.cdf(k=4, mu=5)




#Example 3: Probability where occurence Greater than Some Value

#A certain store sells 15 cans of tuna per day on average. 
#What is the probability that this store sells more than 20 
# cans of tuna in a given day?

from scipy.stats import poisson

#calculate probability
1-poisson.cdf(k=20, mu=15)

















### Lecture 4. Normal distribution

#Histogram plotting Normal Distribution

import numpy as np
import matplotlib.pyplot as plt
  
# Mean of the distribution
Mean = 32
 
# satndard deviation of the distribution
Standard_deviation  = 8
  
# size
size = 3200
  
# creating a normal distribution data
values = np.random.normal(Mean, Standard_deviation, size)
  
# plotting histograph
plt.hist(values, 10)
# plotting mean line
plt.axvline(values.mean(), color='k', linestyle='dashed', linewidth=2)
plt.show()




#Example 1 using Normal Distribution
# Suppose student test scores follow Normal probability distribution.
# with mean 81 and standard deviation 18. 
# Calculate the Percentage of Students who have scores less than 60

# Solution: scipy.stats.norm() 

# import required libraries
from scipy.stats import norm
import numpy as np
 
# Given information
mean = 81
std_dev = 18
total_students = 100
score = 60
 
# Calculate z-score for 60
z_score = (score - mean) / std_dev
 
# Calculate the probability of getting a score less than 60
prob = norm.cdf(z_score)
 
# Calculate the percentage of students who got less than 60 marks
percent = prob * 100

# Print the result
print("Percentage of students who got less than 60 marks:", round(percent, 2), "%")




#Example 2: Calculate the Percentage of Students who have scored 
#More than 95

#To get the percentage of people who have scored more than 95. 
#We first find the probability of people who have scored less than 95 
#then we will subtract the probability from 1 to get the percent of 
# people who have scored more than 95. 

# import required libraries
from scipy.stats import norm
import numpy as np
 
# Given information
mean = 81
std_dev = 18
total_students = 100
score = 95
 
# Calculate z-score for 95
z_score = (score - mean) / std_dev
 
# Calculate the probability of getting a more than 95
prob = norm.cdf(z_score)
 
# Calculate the percentage of students who got more than 95 marks
percent = (1-prob) * 100
# Print the result
print("Percentage of students who got more than /95 marks: ", round(percent, 2), " %")



#Python Code for Percentage of Students who have scored More than 
#75 and less than 85

# import required libraries
from scipy.stats import norm
import numpy as np
 
# Given information
mean = 81
std_dev = 18
total_students = 100
min_score = 75
max_score = 85
 
# Calculate z-score for 75
z_min_score = (min_score - mean) / std_dev
# Calculate z-score for 85
z_max_score = (max_score - mean) / std_dev
 
 
# Calculate the probability of getting less than 70
min_prob = norm.cdf(z_min_score)
 
# Calculate the probability of getting  less than 85
max_prob = norm.cdf(z_max_score)
 
percent = (max_prob-min_prob) * 100
 
# Print the result
print("Percentage of students who got marks between 75 and 85 is", round(percent, 2), "%")




#Find the score under which there are about 80% of the students' scores

# import required libraries
from scipy.stats import norm
import numpy as np
 
# Given information
mean = 81
std_dev = 18
total_students = 100
q_score = 0.8

 

#find the z-value with the cumulative probability 50%
#using norm.ppf() ,which is the inverse of norm.cdf()

z_80 = norm.ppf(q_score)


z_80_score = z_80 * std_dev + mean


z_80_score

#Alternative way

z_80_score = norm.ppf(q_score, loc = mean, scale = std_dev )

z_80_score














### Lecture 5. Shapiro-Wilk test for normality

#NULL hypothesis: Sample is from the normal distributions.(Po>0.05)
#(Rejected): Sample is not from the normal distributions.

#Example 1
# import useful library
import numpy as np
from scipy.stats import shapiro
from numpy.random import randn
 
# Create data
test_data = randn(1000)
 
# conduct the  Shapiro-Wilk Test
shapiro(test_data)         

#The result does not reject the normality hypothesis
#as the p-value > 0.05


#Example 2
## import useful library
import numpy as np
from numpy.random import poisson
from numpy.random import seed
from scipy.stats import shapiro
from numpy.random import randn
 
seed(0)
# Create data
test_data = poisson(5, 200)
 
# conduct the  Shapiro-Wilk Test
shapiro(test_data)

#normality test is rejected , since p-value < 0.05














### Lecture 6. Exponential distribution

from scipy.stats import expon 
import numpy as np
import matplotlib.pyplot as plt

#scale or beta, is the average time between two events
# it is the reciprocal of hazard rate lambda in poisson distribution.
   
# Random Variates
R = expon.rvs(scale = 2,  size = 10)
print ("Random Variates : \n", R)
  

# PDF

quantile = np.arange (0.01, 3, 0.1)

#The threshold parameter defines the lowest possible value in 
#an exponential distribution. Some analysts refer to this parameter
# as the location

#probability density
Den_city = expon.pdf(quantile,  scale = 1)

plt.plot(quantile, Den_city)



#cumulative probability 
quantile = np.arange (0.01, 9, 0.1)
Cum_prob = expon.cdf(quantile,  scale = 1)

plt.plot(quantile, Cum_prob)



















### Lecture 7. Chi-Square test


from scipy.stats import chi2_contingency
 
# defining the table
data = [[32, 20, 17, 10, 7, 3], [56, 39,18, 69, 93, 66]]
stat, p, dof, expected = chi2_contingency(data)
 
# interpret p-value
alpha = 0.05
print("p value is " + str(p))
if p <= alpha:
    print('Dependent (reject H0)')
else:
    print('Independent (H0 holds true)')
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
### Lecture 8. Beta distribution

#Creating beta continuous random variable

# importing scipy
from scipy.stats import beta
import numpy as np
import matplotlib.pyplot as plt
  

#Probability density
#create beta variates
quantile = np.arange (0.01, 1, 0.1)
a = 1
b = 1

R = beta.pdf(quantile, a, b)
print ("\nProbability Distribution : \n", R)

plt.plot(quantile,R)


a = 2
b = 2

R = beta.pdf(quantile, a, b)
print ("\nProbability Distribution : \n", R)

plt.plot(quantile,R)




#Cumulative probabilities

quantile = np.arange (0.01, 1, 0.1)
a = 2
b = 2

R = beta.cdf(quantile, a, b)
print ("\nProbability Distribution : \n", R)

plt.plot(quantile,R)



#getting quantiles from given cumulative probabilities

probs = np.arange (0.01, 1, 0.1)
a = 2
b = 2

R = beta.ppf(probs, a, b)
print ("\nProbability Distribution : \n", R)

plt.plot(probs,R)





## Generating Beta random variates
a = 6
b = 2

R = beta.rvs(a, b, size = 10)
print ("Random Variates : \n", R)


















### Lecture 9. Lognormal distribution
from scipy.stats import lognorm 
import numpy as np 
import matplotlib.pyplot as plt

quantile = np.arange (0.01, 10, 0.5) 
  
a = 5       #this location refers to lognormal variable, not for the 
             #log of lognormal variables
b = 2       #this is the sigma in density function
  
# PDF  probability density
R = lognorm.pdf(quantile, loc=a, s=b) 

plt.plot(quantile, R)


#cdf, cumulative probability
a = 0
b = 1

R = lognorm.cdf(quantile,loc=a, s=b) 
plt.plot(quantile, R)



#ppf to calculate quantiles from given cumulative probabilites
a = 0
b = 1
probs = np.arange (0.01, 1, 0.1)

R = lognorm.ppf(probs,loc=a, s=b) 
plt.plot(probs, R)




# Random Variates 
N = 32
a = 5
b = 2

R = lognorm.rvs(loc=a, s=b, size=N) 
print(R) 























### Lecture 10. Gamma distribution

from scipy.stats import gamma 
import numpy as np 
import matplotlib.pyplot as plt


quantile = np.arange (0.01, 10, 0.5) 
  
a = 1       # parameter a (shape)
b = 1       # parameter scale 
  
# PDF  probability density
# pdf(quantile, a , scale) 

R = gamma.pdf(quantile, a = a, scale=b) 

plt.plot(quantile, R)



#cdf, cumulative probability
# cdf(quantile, a , scale)

a = 1
b = 1

R = gamma.cdf(quantile,a=a, scale=b) 
plt.plot(quantile, R)



#ppf to calculate quantiles from given cumulative probabilites
a = 1
b = 1
probs = np.arange (0.01, 1, 0.1)

R = gamma.ppf(probs,a=a, scale=b) 
plt.plot(probs, R)




# Random Variates generation
N = 32
a = 3
b = 2

R = gamma.rvs(a=a, scale=b, size=N) 
print(R) 


















### Lecture 11. t distribution

from scipy.stats import t
import numpy as np 
import matplotlib.pyplot as plt



# PDF  probability density
# pdf(quantile, df , loc)
#loc default 0 

quantile = np.arange (-8, 8, 0.1) 
df = 10
loc = 2

R = t.pdf(quantile, df=df) 

plt.plot(quantile, R)


R = t.pdf(quantile, df=df, loc=loc) 

plt.plot(quantile, R)



#cdf, cumulative probability
# cdf(quantile, df , loc)

quantile = np.arange (-8, 8, 0.1) 
df = 10
loc = 2

R = t.cdf(quantile,df=df, loc=loc) 
plt.plot(quantile, R)


#ppf to calculate quantiles from given cumulative probabilites
df = 10
loc = 2
probs = np.arange (0.01, 1, 0.01)

R = t.ppf(probs,df=df, loc=loc) 
plt.plot(probs, R)



#random number generation using t.rvs()

test1 = t.rvs(10, size= 10000)


fig, ax = plt.subplots(figsize =(12, 9))
#ax.hist(test, bins = [-4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
ax.hist(test1, bins = 50)
  
# Show plot
plt.show()


test2 = t.rvs(df=10, loc = 5, size= 10000)


fig, ax = plt.subplots(figsize =(12, 9))
#ax.hist(test, bins = [-4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
ax.hist(test2, bins = 50)
  
# Show plot
plt.show()



##Example of t-test of population mean

# A chemical engineer claims that the population mean yield of 
#a certain batch process is 500 grams per milliliter of raw material.
#To check this claim he samples 25 batches each month. 
#If the computed t-value falls between -t0.05 and t0.05, he is 
#satisfied with this claim. What conclusion should he draw from 
#a sample that has a mean X-Ba = 518 grams per milliliter and 
#a sample standard deviation s = 40 grams? Assume the distribution 
# of yields to be approximately normal.

#Solution : 
# first we get the critical value of t0.05 
# using qt(probs, df)
import math

t_cri = t.ppf( 1-0.05, 24)
t_cri   #1.71

# then we calculate the sample t test statistics
t_sample = (518 - 500) / (40/math.sqrt(25))
t_sample     #2.25

#we can also calcualte the p_value assocated with t_sample
p_sample = 1 - t.cdf(t_sample, 24)
p_sample   #0.0169


#As t_sample > t_cri, which also means p_sample < 0.05, 
# engineer is likely to conclude that the process produces 
# a better product than he thought, i.e. larger than 500 

















### Lecture 12. Normal quantile-quantile plot

import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
  
# np.random generates different random numbers
# whenever the code is executed

  
# Random data points from normal distribution generated
data_points = np.random.normal(6, 2, 100)    
  
sm.qqplot(data_points, line='s')
plt.show()



# Random data points from logistic distribution generated
data_points = np.random.logistic(size=100) 

sm.qqplot(data_points, line='s')
plt.show()
















### Lecture 13. Descriptive Statistics of Pandas DataFrame
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
 
# create dataframe
data_set = {'Familymember': ['Wilson', 'Shirley', 'Dudu', 'Maomao', 'Miaomiao', 'Mico', 'Mia', 'Mimi'],
            'Age' : [32,33,20, 22, 10,7, 3, 22],
            'Income' : [32000, 26000, 20000, 22000, 10000, 18000, 13000,20000 ],
            'Cost' : [28000, 20000, 15000, 17000, 8000, 12000, 6000,8000]
            }
 
frame = pd.DataFrame(data_set)
frame
 
# Describing descriptive statistics of Income
print("\nDescriptive statistics of Income:\n")
stats = frame['Income'].describe()
print(stats)



# Describing descriptive statistics of whole dataframe
print("\nDescriptive statistics of whole dataframe:\n")
stats = frame.describe(include = 'all')
print(stats)



#calculate the descriptive statistical data individually.

# Print Count of Income
print("\nCount of Income:\n")
counts = frame['Income'].count()
print(counts)
  
# Print mean of Income
print("\nMean of Income:\n")
m = frame['Income'].mean()
print(m)
  
# Print maximum value of Income
print("\nMaximum value of Income:\n")
mx = frame['Income'].max()
print(m)
  
# Print standard deviation of Income
print("\nStandard deviation of Income:\n")
sd = frame['Income'].std()
print(sd)
















### Lecture 14. Create Frequency Tables

# Simple frequency table using value_counts() method

import pandas as pd
import numpy as np

data_set = {'Familymember': ['Wilson', 'Shirley', 'Dudu', 'Maomao', 'Miaomiao', 'Mico', 'Mia', 'Mimi'],
            'Age' : [32,33,20, 22, 10,7, 3, 22],
            'Gender': ['male','female','male','female','male','female','male','male'],
            'City': ['Molde','Aukra','Molde','Molde','Aukra','Molde','Aukra','Molde'],
            'Income' : [32000, 26000, 20000, 22000, 10000, 18000, 13000,20000 ],
            'Cost' : [28000, 20000, 15000, 17000, 8000, 12000, 6000,8000]
            }
 
frame = pd.DataFrame(data_set)
frame


Gender_count = frame['Gender'].value_counts()
print(Gender_count)     
type(Gender_count)      #a Series


#One-way frequency table using pandas.crosstab() method

freq_table = pd.crosstab(frame['Gender'], 'Sex')
  
freq_table        

type(freq_table)        #a dataframe



#show frequency table to be in proportions

freq_table = pd.crosstab(frame['Gender'], 'Sex')
  
# frequency table in proportion of species
freq_table= freq_table/len(frame)
  
freq_table




#Two-way frequency table using pandas.crosstab() method

freq_table = pd.crosstab(frame['Gender'], frame['City'])
  
freq_table


















### Lecture 15. Confidence interval of population mean using t distribution 

import numpy as np
import scipy.stats as st


# define sample data
Income = [32000, 26000, 20000, 22000, 10000, 18000, 13000,20000, 56000, 32000, 27000, 28000, 19000, 30100 ]
  
# create 95% confidence interval
#using st.t.interval()
st.t.interval(alpha=0.95, df=len(Income)-1,
              loc=np.mean(Income),
              scale=st.sem(Income))


## Calculate confidence interval step by step
sample_mean = np.mean(Income)

alpha = 0.05

df = len(Income)-1

 
sample_sd = np.std(Income) * np.sqrt(len(Income)/(len(Income)-1))

samplemean_sd = sample_sd / np.sqrt(len(Income))


t_value =  st.t.ppf(1-alpha/2,df=df)


Lower_limit = sample_mean - t_value * samplemean_sd

Upper_limit = sample_mean + t_value * samplemean_sd

print("Confidence Interval 100(1-0.05%")

print([Lower_limit, Upper_limit])


























### Lecture 16. Levene’s Test for homogeneity of variance

import pandas as pd
import numpy as np
import scipy.stats as stats

group1 = [7, 14, 14, 13, 12, 9, 6, 14, 12, 8]
group2 = [15, 17, 13, 15, 15, 13, 9, 12, 10, 8]
group3 = [6, 8, 8, 9, 5, 14, 13, 8, 10, 9]


alpha = 0.05
# Levene's test centered at the mean
lv_stats, p_value = stats.levene(group1, group2,group3, center='mean')
 
if p_value > alpha:
    print("We do not reject the null hypothesis")
else:
    print("Reject the Null Hypothesis")

lv_stats

p_value



# Levene's test centered at the median
lv_stats, p_value = stats.levene(group1, group2,group3, center='median')
 
if p_value > alpha:
    print("We do not reject the null hypothesis")
else:
    print("Reject the Null Hypothesis")


lv_stats

p_value



#example with pandas dataframe

data_set = {'Familymember': ['Wilson', 'Shirley', 'Dudu', 'Maomao', 'Miaomiao', 'Mico', 'Mia', 'Mimi'],
            'Age' : [32,33,20, 22, 10,7, 3, 22],
            'Gender': ['male','female','male','female','male','female','male','male'],
            'City': ['Molde','Aukra','Molde','Molde','Aukra','Molde','Aukra','Molde'],
            'Income' : [32000, 26000, 20000, 22000, 10000, 18000, 13000,20000 ],
            'Cost' : [28000, 20000, 15000, 17000, 8000, 12000, 6000,8000]
            }
 
frame = pd.DataFrame(data_set)
frame


female_income = frame[frame['Gender']=='female']['Income']

male_income = frame[frame['Gender']=='male']['Income']

#Levene test
stat, p_value = stats.levene(female_income, male_income, center='mean')

print(f"Lavene's test statistic: {stat}")
print(f"P-value: {p_value}")

















### Lecture 17. Point Biserial Correlation with Python

import pandas as pd
import numpy as np
from scipy.stats import pointbiserialr

data_set = {'Familymember': ['Wilson', 'Shirley', 'Dudu', 'Maomao', 'Miaomiao', 'Mico', 'Mia', 'Mimi'],
            'Age' : [32,33,20, 22, 10,7, 3, 22],
            'Gender': ['male','female','male','female','male','female','male','male'],
            'City': ['Molde','Aukra','Molde','Molde','Aukra','Molde','Aukra','Molde'],
            'Income' : [32000, 26000, 20000, 22000, 10000, 18000, 13000,20000 ],
            'Cost' : [28000, 20000, 15000, 17000, 8000, 12000, 6000,8000]
            }
 
frame = pd.DataFrame(data_set)
frame

frame = frame.replace(['male','female'],[0,1])

pbc = pointbiserialr(frame['Gender'],frame['Income'])
print(pbc)  

























### Lecture 18. Mann-Whitney U Test 
import scipy.stats as stats
mpg_group1 = [20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19]
mpg_group2 = [24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23]


# mannwhitneyu(x, y, use_continuity=True)

# where:

# x: an array of sample observations from group 1
# y: an array of sample observations from group 2
# use_continuity: whether a continuity correction 1/2
# should be taken into account. Default is True.


#perform the Mann-Whitney U test
stats.mannwhitneyu(mpg_group1, mpg_group2, alternative='two-sided')

#the Mann-Whitney U Test uses the following null and alternative hypotheses:

# H0: The mpg is equal between the two groups

# HA: The mpg is not equal between the two groups

# Since the p-value (0.2114) is not less than 0.05, we fail to
# reject the null hypothesis.

# This means we do not have sufficient evidence to say that the 
# true mean mpg is different between the two groups.






















### 19. F distribution and F-test 

from scipy.stats import f
import numpy as np 
import matplotlib.pyplot as plt
import scipy



# PDF  probability density
# f.pdf(quantile, df1,df2)


x = np.linspace(0, 5, 100)
  
# Varying positional arguments
y1 = f.pdf(x, 2, 6)
plt.plot(x, y1, "*")


#cdf, cumulative probability
# cdf(quantile, df1,df2)


y2 = f.cdf(x, 2, 6) 
plt.plot(x, y2, "*")


#ppf to calculate quantiles from given cumulative probabilites

probs = np.arange (0.01, 1, 0.01)

y3 = f.ppf(probs,2,6) 
plt.plot(probs, y3,"*")



#random number generation using f.rvs()

y4 = f.rvs(2,6, size=10000)


fig, ax = plt.subplots(figsize =(12, 9))
ax.hist(y4, bins = [0, 0.5, 1.5, 2.5, 3.5, 4.5,5.0,5.5,6.0,6.5,7,7.5,8])

  
# Show plot
plt.show()



## F-Test

# Create data
group1 = [0.28, 0.2, 0.26, 0.28, 0.5]
group2 = [0.2, 0.23, 0.26, 0.21, 0.23]
  
# converting the list to array
x = np.array(group1)
y = np.array(group2)
  
# calculate variance of each group
print(np.var(group1), np.var(group2))
  
def f_test(group1, group2):
    f = np.var(group1, ddof=1)/np.var(group2, ddof=1)
    nun = x.size-1
    dun = y.size-1
    p_value = 1-scipy.stats.f.cdf(f, nun, dun)
    return f, p_value
  
# perform F-test
f_test(x, y)





























