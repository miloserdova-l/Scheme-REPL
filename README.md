### An implementation of Scheme mini-language (+ Call-with-current-continuation)

This is a homework for functional programming course.

Author: Lyubov Miloserdova, miloslubov@gmail.com

Features done:

- AST
- Parser (+ tests)
- Interpreter (+ tests)
    - ``` +, -, *, /, <, >, <=, >=, = ``` with numbers 
    - ``` car, cdr ``` with lists
    - added environment (declared functions and variables)
    - function and variable declaration
    - lambdas
    - Quote/Unquote
    - Call/CC
  
      Примечание: для обработки call/cc к контексту добавляется функция id от переменной " x" (имя функции эквивалентно параметру лямбды). Никаких проблем с перекрытием переменных не возникает. 
- REPL

Features in progress (and TODOs):
 
- stdlib


### Run:

#### Install sbt. 

First add the necessary repository with the command:

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
```

Add the public key for the installation with the command:

```
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
```

Update apt with the command:

```
sudo apt-get update
```

Finally, install sbt with the command:

```
sudo apt-get install sbt -y
```

#### Run main():

```
sbt run
```

#### Run tests:

```
sbt test
```
