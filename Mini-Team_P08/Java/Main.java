
import java.util.Scanner;
import java.util.LinkedList;

/**
 * Realiza el analisis lexico de una cadena y devuelve una lista de lexemas basada en el Automata
 * definido. Esto apartir de un archivo o desde cadenas ingresadas desde la terminal.
 * @since Automata, ManejadorArchivo
 * @version 1.0
 */
public class Main{

    /**
     * 't' para ingresar las cadenas desde la terminal, y 'a' para obtener las cadenas
     * desde un archivo(sin comillas). Esto se te ira pidiendo conforme se va ejecutando.
     * @param args Sin uso.
     */
    public static void main(String[] args){

	Scanner leer = new Scanner(System.in);
	Automata analizador;

	System.out.println("Teclea 't' para ingresar las expresiones desde terminal o" + 
                           "teclea 'a' para poder leer las expresiones de un archivo.");
	String tipo = leer.nextLine();

	switch(tipo){

	case "t":
	    String expresion = "";
	    String opcion = "";
	    do{
		System.out.println("Ingresa tu expresión: \n");
		expresion = leer.nextLine();
		analizador = new Automata(expresion);
		analizador.procesa();
		System.out.println("¿Quieres ingresar otra expresión(s/n): ");
		opcion = leer.nextLine();
	    }while(opcion.equals("s"));	    
	    break;
	case "a":
	    String nombre = "";
	    ManejadorArchivo nuevo;
	    System.out.println("Ingresa el nombre del archivo: ");
	    nombre = leer.nextLine();
	    nuevo  = new ManejadorArchivo(nombre);
	    LinkedList<String> expresiones = nuevo.leerExpr();
	    for(String expr : expresiones){
		System.out.println("\n" + expr);
		analizador = new Automata(expr);
		analizador.procesa();
	    }	    
	    break;	    
	default:
	    System.out.println("Opción no valida.");
	}	
    }
}
