# Lenguaje Pau Patrol++
Pau Patrol++, un lenguaje orientado a objetos que provee mecanismos para definir clases, atributos, métodos y herencia sencilla, basado en C++.

# Manual de usuario

## Instalación

Para poder correr el programa es necesario tener instalado Python 3 y Ply. 
Para instalar python ingresa a este [link](https://www.python.org/downloads/)
Para instalar ply, una vez instalado python, corre el siguiente comando en tu terminal
```bash
pip install ply
```


Una vez instalados descarga el repositorio!

## ¡Crea tu archivo .patrol !
Para poder correr tu programa, deberás de crear un archivo con la extensión “.patrol”. Te recomendamos que lo hagas dentro de la carpeta del repositorio.

## Estructura del programa
Un programa Patrol empieza con la declaración del nombre del programa, seguido por todas sus clases, la declaración de variables globales, funciones y al ultimo la función main del programa. Los únicos bloques de código obligatorios son la declaración del nombre del programa y la función main del programa. A continuación te presentamos un programa básico de ejemplo:
```bash
program Prog1;
main(){
    write("Hello World");

}
```

## Tipos primitivos
Pau Patrol ++ soporta `int`, `float`, `char` y `bool` como tipos de variables primitivos
## Variables
### Sintaxis adecuado para variables
Las variables deben empezar con letra minúscula, el resto de su contenido puede contener letras minúsculas, mayúsculas, números y guiones bajos
#### Sintaxis correcta
`a` `nombreVariable` `nombre_Variable` `a1`
#### Sintaxis incorrecta
`A` `1a` `__variable__`

### Declaración de variables globales 
La declaración de variables globales debe empezar con el encabezado `vars`
```bash
vars
var: int;
```
### Declaración de variables locales
La declaración de variables locales dentro de funciones sigue la misma estructura que una variable global pero no hay que incluir el encabezado `vars`
### Declaración de arreglos y matrices
Un arreglo o matriz es declarado dándole un nombre de variable seguido por dos corchetes en donde se especificarán las dimensiones de la variable
#### Arreglos
```
Arr[5] : int;
```
#### Matrices
```
Mat[5,10] : int;
```

## Bloques de codigo

### Sintaxis correcta para nombres de funciones, clases y nombre del programa
Estos ids deben empezar con una letra mayúscula y pueden contener letras mayúsculas, minúsculas y número
#### Sintaxis correcta
`A` `Programa` `Func1`
#### Sintaxis incorrecta
`a` `5Function` `Func_1`

### Declaración de nombre del programa
Este bloque debe ser el primero en tu codigo, empieza con la palabra `program`, seguido por el nombre de tu programa y termina con un `;`
```bash
program Prog1;
```
### Expresiones 
#### Operadores aritméticos
| Operación | Operador |
| ------------- | ------------- |
| Suma  | +  |
| Resta  | -  |
| Multiplicación  | *  |
| División  | /  |
#### Operadores aritméticos
| Operación | Operador |
| ------------- | ------------- |
| Mayor que  | >  |
| Menor que  | <  |
| Mayor igual que  | >=  |
| Menor igual que  | <=  |
| Igual  | ==  |
| Diferente  | !=  |
| And  | &  |
| Or  | \|  |
#### Operadores aritméticos
| Operación | Operador |
| ------------- | ------------- |
| Asignación  | =  |
| Suma igual  | +=  |
| Resta igual  | -=  |
| Multiplicación igual  | \*=  |
| División igual  | /=  |

### Asignación
Se puede usar cualquiera de los operadores de asignación para asignar el resultado de una expresión a una variable, ya sea sencilla o dimensionada. Si se usa un operador que implica una operación extra, es necesario que la variable a la que se está asignando un valor tenga un valor previamente definido.
#### Asignación a una variable sencilla
```bash
var1 = 5;
var2 = 9+6;
```
#### Asignación a una variable dimensionada
```bash
var[5] = 10;
var[2,3] = 15*2;
```
### Return
La estructura de la función return empieza con la palabra `return` seguido por dos paréntesis en donde pondremos la expresión que se desea retornar.
```bash
return(a+b);
return(5);
```
### Llamada a función
Para llamar una función es necesario empezar con el nombre de la función a llamar, seguido por sus parámetros entre paréntesis.
```bash
Func1();
Func2(a,b,9);
```
### Write
Esta función imprimirá variables, valores y letreros en consola. La estructura de la función write empieza con la palabra `write` seguido por dos paréntesis en donde podremos colocar todo aquello que queremos que se imprima. Para imprimir más de una expresión dentro de la función es necesario separarlas por comas.
```bash
write(a);
write("Hello World", 5+10, b);
```
### Read
Esta función recibirá un valor de un usuario y la asignará a la variable definida. La estructura de la función read empieza con la palabra `read` seguido por dos paréntesis en donde pondremos colocar la función a la cual queremos dar un valor.
```bash
read(a);
```
### Estatuto condicional If
Pau Patrol++ soporta los bloques condicionales `if` y `else`, basandose un poco en la estructura de c++, el bloque `if` puede o no ser seguido por un bloque `else`.
#### Estructura de condicional solo con if
```bash
if(r == 1) then {
  write(r);
}
```
#### Estructura de condicional if-else
```bash
if(r == 1) then {
  write(r);
} else{
  write(5);
}
```
### Estatuto condicional While
Pau Patrol++ también soporta el bloque condicional `while`, basándose un poco en la estructura de c++.
#### Estructura de condicional while
```bash
while(a > b) do {
  write(5);
} 
```
### Estatuto no condicional From
Pau Patrol++ soporta el bloque no condicional `from`, basandose en la estructura del for loop de c++, donde primero se hace una asignación y luego se define un limite para la variable.
#### Estructura de no condicional from
```bash
from x = 0 to l0 do{
  write(x);
}
```
## Funciones
Las funciones dentro de Pau Patrol++ siguen la siguiente estructura, siendo la declaración de variables locales opcional.
```
Tipo_Funcion function Nombre_Función(Parametros);
Declaración de variables
{
  Bloque de codigo
}
```
### Tipos de funciones
Los tipos que puede tomar una función son `int`, `float`, `char`, `bool` y `void`
### Parametros
Una función puede o no tener definidos parametros de entrada. La sintaxis para definir un parámetro es la siguiente `nombre_variable : tipo_Variable;`, al final de la declaración de un parámetro es necesario colocar un `;`
#### Ejemplo de definición de parámetros
```
int function Func1(a:int;);
```
```
void function Func2(a:int; b:float;)
```
```
int function Func3();
```
## Clases
Las clases dentro de Pau Patrol++ siguen la siguiente estructura, siendo la declaración de atributos opcional.
```
class Nombre_Clase; {
  attributes
  declaración de variables
  methods
  bloque de funciones
};
```
### Declaración de atributos
La declaración de atributos en una clase es opcional, debe estar definida por el encabezado de `attributes` y dentro sigue la misma estructura que una declaración de variables normal.
```bash
 attributes
    age: int;
    weight: float;
    size: char;
```
### Declaración de métodos
Dentro de la declaración de métodos es obligatoria dentro de las clases y debe tener al menos una función definida.
```bash
 methods
    void function ClassFunc();
    {
        age = 0;
        weight = 0.0;
        size = 's';
    }
```
### Herencia de clases
Pau Patrol++ soporta la herencia entre clases. Para heredar una clase esta debe estar definida con anterioridad y la sintaxis a seguir para heredar una clase es la siguiente:
```
class Hijo <inherit Padre>; {
```
### Definición de objetos
Para definir un objeto de una clase se hace de la siguiente manera:
```
var : NombreClase;
```
#### Llamada de variable de un objeto
Para obtener o asignar el valor de una variable definida de en una clase se sigue la siguiente sintaxis:
```
var.varClass = 5;
```
```
a = var.varClass + 5;
```
#### Llamada de función de un objeto
Para mandar a llamar a una método definido en una clase se sigue la siguiente sintaxis:
```
var.ClassFunc1();
```
```
var.ClassFunc2(a,2.5);
```
## Comentarios
Para colocar un comentario dentro de tu programa es necesario poner el simbolo `#` seguido de tu comentario.
```
#Esto es un comentario
a = 5; #Esto tambien es un comentario
```



## Correr tu programa
Una vez que hayas terminado de escribir tu programa, guarda el archivo.
Abre la terminal y ve a la carpeta donde tienes el archivo.
Una vez ahi ejecuta el siguiente comando:
```bash
python3 LaTonta.py
```

En caso de estar en otra carpeta
```bash
python3 [direccion del repo]/LaTonta.py
```
Y listo! Tendrás a tu pequeño programa en Pau Patrol++ corriendo. 
¡RECORDATORIO! : Si hay errores en el programa estos aparecerán en la terminal cuando lo corras


## Video Tutorial
A continuación te dejamos un video tutorial de como hacer y correr tus programas Pau Patrol ++
[Video](https://www.loom.com/share/4c325f068fe64cdd8c4220f848fc9e30 )


## Documentación

Si quieres saber mas sobre el lenguaje y el compilador de para Pau Patrol++ puedes checarla documentación [aquí](https://docs.google.com/document/d/1N0BVC8eclY30qOEl0np-VVmWELTVsMfUdcGEdfVGKYc/edit?usp=sharing)

## Autores
* Gabriel Ortega Jacobo
* Paulina Cámara Vidales

