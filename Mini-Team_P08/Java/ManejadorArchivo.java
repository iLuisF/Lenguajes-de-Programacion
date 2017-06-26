
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;
import java.util.LinkedList;

/**
 * En esta clase se implementa los métodos necesarios para leer los datos de un
 * archivo y escribir los datos en un archivo.
 *
 * @author Flores González Luis Brandon.
 * @version 1.1
 */
public class ManejadorArchivo {

    private String leerArchivo;

    /**
     * Constructor que solo inicia el nombre de un archivo para ser leido.
     *
     * @param leerArchivo Nombre del archivo para leer.
     */
    public ManejadorArchivo(String leerArchivo) {
        this.leerArchivo = leerArchivo;
    }


    /**
     * Constructor en caso de solo querer usar el método escribir.
     */
    public ManejadorArchivo(){
    }

    /**
     * Método para escribir en un archivo una cadena dada. Se usa FileWriter.
     *
     * @param cadena Cadena que se escribira en el archivo.
     * @param nombreArchivo Nombre del archivo que se usara para para escribir
     * la cadena. En caso de que no exista se creera y en caso de que si exista
     * se sobreescribira su contenido.
     */
    public void escribir(String cadena, String nombreArchivo) {
        FileWriter escritor = null;
        try {
            escritor = new FileWriter(nombreArchivo);
            escritor.write(cadena);
        } catch (IOException excepcion) {
            System.out.println(excepcion.getMessage());
        } finally {
            if (escritor != null) {
                try {
                    escritor.close();
                } catch (IOException excepcion) {
                    System.out.println(excepcion.getMessage());
                }
            }
        }
    }

    /**
     * Lee una serie de expresiones, donde cada expresión esta en una linea.
     * 
     * @return Lista de expreciones.
     */
    public LinkedList<String> leerExpr(){
	LinkedList<String> expresiones = new LinkedList<String>();
        try {
            Scanner leer = new Scanner(new File(leerArchivo));
            while (leer.hasNext()) {
		expresiones.add(leer.nextLine());
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Archivo no encontrado :(.\n" + ex.getMessage());
        }
	return expresiones;
    }

}

