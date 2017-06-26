import java.util.LinkedList;

public class Automata{
	
    String cadena_actual; // estado en donde estamos parados
    int estado_actual;
    char[] palabra; // la palabra que vamos a procesar 
    LinkedList<String> lexemas;

    /*
     * inicializamos el automata con el estado incial q0;
     */
    public Automata(String pal){
	if(!pal.endsWith("}")){
	    pal += " ";
	}
	this.palabra = pal.toCharArray();
	this.cadena_actual = "";
	this.estado_actual = 0;
    }

    public void cambiaPalabra(String nva_palabra){
	this.palabra = nva_palabra.toCharArray();
	this.cadena_actual = "";
    }

    public void  FuncionTransicion(char actual){
	if(actual == '{' && cadena_actual == ""){
	    System.out.println("llave : {");
	    estado_actual = 1;
	}else{
	    char[] aux = new char[1];
	    aux[0] = actual;
	    cadena_actual += new String(aux);
	    switch(estado_actual){
		//rama que acepta los id y verifica las llaves
	    case 0:
		switch(actual){
		case ' ':
		    if((int)cadena_actual.toCharArray()[0] != 32){
			System.out.println("identificador : " + cadena_actual);
								
		    }
		    cadena_actual = "";
		    estado_actual = 0;
		    break;
		case 't':
		    estado_actual = 2;
		    break;
		case '-':
		    estado_actual = 3;
		    break;
		case 'f':
		    estado_actual = 17;
		    break;
		case '1':
		case '2':
		case '3':
		case '4':
		case '6':
		case '7':
		case '8':
		case '9':
		case '0':
		    estado_actual = 3;
		    break;
		case 'e':
		    estado_actual = 4;
		    break;
		case '{':
		    System.err.println("token no definido : " + cadena_actual);
		    cadena_actual = "";
		    estado_actual = 0;
		    break;
							
		case '}':
		    switch(cadena_actual.toCharArray()[0]){
		    case ' ':
			break;
		    case '}':
			System.out.println("llave : " + cadena_actual);
			cadena_actual = "";
			estado_actual = 0;
			break;
		    default:
			System.out.println("identificador : " + cadena_actual.substring(0,cadena_actual.length()-1));
			System.out.println("llave : }");
			cadena_actual = "";
		    }
		    break;
		default:
		    estado_actual = 0;
		    break;
		}
		break;
		// rama que acepta los numeros
	    case 3:
		switch(actual){
		case '1':
		case '2':
		case '3':
		case '4':
		case '6':
		case '7':
		case '8':
		case '9':
		case '0':
		    estado_actual = 3;
		    break;
		case ' ':
		    System.out.println("numero : " + cadena_actual);
		    cadena_actual = "";
		    estado_actual = 0;
		    break;
		case '}':
		    System.out.println("numero : " + cadena_actual.substring(0,cadena_actual.length()-1));
		    System.out.println("llave : }");
		    cadena_actual = "";
		    estado_actual = 0;
		    break;
		default:
		    estado_actual = 0;
		}
		//estado que manda lo que hay despues de  un {
	    case 1:
		switch(actual){
		case 'c':
		    estado_actual = 5;
		    break;
		case 'i':
		    estado_actual = 15;
		    break;
		case 't':
		    estado_actual = 2;
		    break;
		case 'f':
		    estado_actual = 17;
		    break;
		case '1':
		case '2':
		case '3':
		case '4':
		case '6':
		case '7':
		case '8':
		case '9':
		case '0':
		    estado_actual = 3;
		    break;
		case 'e':
		    estado_actual = 4;
		    break;
		case 'r':
		    estado_actual = 26;
		    break;
		case 'w':
		    estado_actual = 29;
		    break;
		case '{':
		    System.out.println("llave : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '+':
		case '-':
		case '/':
		case '*':
		case '%':
		case '=':
		    estado_actual = 33;
		    break;
		case '>':
		case '<':
		case '!':
		    estado_actual = 34;
		    break;
		case 'm':
		    estado_actual = 36;
		    break;
		case 'n':
		    estado_actual = 42;
		    break;
		case 'p':
		    estado_actual = 49;
		    break;
		case 'a':
		    estado_actual = 40;
		    break;
		case 'o':
		    estado_actual = 39;
		    break;
		case 'z':
		    estado_actual = 60;
		    break;
		case 'h':
		    estado_actual = 51;
		    break;
		case 'l':
		    estado_actual = 57;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama del cons
	    case 5:
		switch(actual){
		case 'o':
		    estado_actual = 6;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 6:
		switch(actual){
		case 'n':
		    estado_actual = 7;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 7:
		switch(actual){
		case 's':
		    estado_actual = 23;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 23:
		switch(actual){
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : cons");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		case ' ':
		    estado_actual = 0;
		    System.out.println("palabrea reservada : " + 	cadena_actual);
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;

		//rama que acepta el lexema true
	    case 2:
		switch(actual){
		case 'r': 
		    estado_actual = 8;
		    break;
		case 'a':
		    estado_actual = 55;
		default:
		    estado_actual = 0;
		}
		break;
	    case 8 :
		switch(actual){
		case 'u': 
		    estado_actual = 9;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 9 :
		switch(actual){
		case 'e': 
		    estado_actual = 10;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 10 :
		switch(actual){
		case ' ': 
		    estado_actual = 0;
		    System.out.println("boolean : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    System.out.println("boolean : true");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    estado_actual = 0;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		//rama que acepta empty
	    case 4:
		switch(actual){
		case 'm': 
		    estado_actual = 11;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 11 :
		switch(actual){
		case 'p': 
		    estado_actual = 12;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 12 :
		switch(actual){
		case 't': 
		    estado_actual = 13;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 13 :
		switch(actual){
		case 'y': 
		    estado_actual = 14;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 14:
		switch(actual){
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : empty");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		case ' ': 
		    estado_actual = 0;
		    System.out.println(cadena_actual);
		    cadena_actual = "";
		    break;
		case '?':
		    estado_actual = 25;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama que verifica el if
	    case 15:
		switch (actual) {
		case 'f':
		    estado_actual = 16;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 16:
		switch(actual){
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : if");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		case ' ': 
		    estado_actual = 0;
		    System.out.println("palabra reservada : " + cadena_actual);
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama que verifica si es fa para mandarlo a la rama del false si es fu para mandarlo a la rama  de fun
	    case 17:
		switch(actual){
		case 'a':
		    estado_actual = 18;
		    break;
		case 'u':
		    estado_actual = 22;
		default:
		    estado_actual = 0;
		}
		break;
		//rama del false
	    case 18:
		switch(actual){
		case 'l':
		    estado_actual = 19;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 19:
		switch(actual){
		case 's':
		    estado_actual = 20;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 20:
		switch(actual){
		case 'e':
		    estado_actual = 21;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 21:
		switch(actual){
		case ' ':
		    estado_actual = 0;
		    System.out.println("boolean : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("boolean : false");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama que acepta fun
	    case 22:
		switch(actual){
		case 'n':
		    estado_actual = 24;
		default:
		    estado_actual = 0;
		}
		break;

	    case 24:
		switch(actual){
		case ' ':
		    estado_actual = 0;
		    System.out.println("palabra reservada : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : fun");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama para aceptar empty?
	    case 25:
		switch(actual){
		case ' ':
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("operador : empty?");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		//rama que acepta rec
	    case 26:
		switch(actual){
		case 'e':
		    estado_actual = 27;
		default:
		    estado_actual = 0;
		}
		break;
	    case 27:
		switch(actual){
		case 'c':
		    estado_actual = 28;
		default:
		    estado_actual = 0;
		}
		break;
	    case 28:
		switch(actual){
		case ' ':
		    estado_actual = 0;
		    System.out.println("palabra reservada : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : rec");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama del with
	    case 29:
		switch(actual){
		case 'i':
		    estado_actual = 30;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 30:
		switch(actual){
		case 't':
		    estado_actual = 31;
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 31:
		switch(actual){
		case 'h':
		    estado_actual = 32;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 32:
		switch(actual){
		case ' ':
		    estado_actual = 0;
		    System.out.println("palabra reservada : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("palabra reservada : with");
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
		// rama de los operadores 
	    case 33:
		switch(actual){
		case '}':
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual.toCharArray()[0]);
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		case ' ': 
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual);
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 34:
		switch(actual){
		case '=':
		    estado_actual = 35;
		    break;
		case ' ': 
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual.toCharArray()[0]);
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 35:
		switch(actual){
		case ' ': 
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual.toCharArray()[0]);
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
	    case 36:
		switch(actual){
		case 'a':
		    estado_actual = 37;
		    break;
		case 'i':
		    estado_actual = 37;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 37:
		switch(actual){
		case 'n':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 38:
		switch(actual){
		case ' ': 
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual);
		    cadena_actual = "";
		    break;
		case '}':
		    estado_actual = 0;
		    System.out.println("operador : " + cadena_actual.substring(0,cadena_actual.length()-1));;
		    System.out.println("llave : }");
		    cadena_actual = "";
		    break;
		default:
		    estado_actual = 0;
		}
		break;
	    case 39:
		switch(actual){
		case 'r':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 40:
		switch(actual){
		case 'n':
		    estado_actual = 41;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 41:
		switch(actual){
		case 'd':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 42:
		switch(actual){
		case 'o':
		    estado_actual = 43;
		    break;
		case 'u':
		    estado_actual = 44;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 43:
		switch(actual){
		case 't':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 44:
		switch(actual){
		case 'm':
		    estado_actual = 45;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 45:
		switch(actual){
		case 'b':
		    estado_actual = 46;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 46:
		switch(actual){
		case 'e':
		    estado_actual = 47;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 47:
		switch(actual){
		case 'r':
		    estado_actual = 48;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 48:
		switch(actual){
		case '?':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 49:
		switch(actual){
		case 'o':
		    estado_actual = 50;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 50:
		switch(actual){
		case 'w':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 51:
		switch(actual){
		case 'e':
		    estado_actual = 52;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 52:
		switch(actual){
		case 'a':
		    estado_actual = 53;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 53:
		switch(actual){
		case 'd':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 55:
		switch(actual){
		case 'i':
		    estado_actual = 56;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 56:
		switch(actual){
		case 'l':
		    estado_actual = 38;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 57:
		switch(actual){
		case 'i':
		    estado_actual = 58;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 58:
		switch(actual){
		case 's':
		    estado_actual = 59;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 59:
		switch(actual){
		case 't':
		    estado_actual = 48;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 60:
		switch(actual){
		case 'e':
		    estado_actual = 61;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 61:
		switch(actual){
		case 'r':
		    estado_actual = 62;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    case 62:
		switch(actual){
		case 'o':
		    estado_actual = 48;
		    break;
		default:
		    estado_actual = 0;
						
		}
		break;
	    }
	}
    }

    public void procesa(){
	for(int i  = 0 ; i < palabra.length ; i++){
	    FuncionTransicion(palabra[i]);
	}
    }
}
