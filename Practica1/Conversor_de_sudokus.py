def convertir_cadena_a_prolog(nombre, cadena):
    """
    Convierte una cadena de 81 dígitos de Sudoku en el formato Prolog especificado.
    Args:
        nombre (str): Nombre del Sudoku en Prolog (por ejemplo, 'sudoku14').
        cadena (str): Cadena de 81 caracteres representando el Sudoku ('0' para casillas vacías).
    """
    if len(cadena) != 81:
        raise ValueError("La cadena debe contener exactamente 81 caracteres.")
    
    # Convierte los caracteres de la cadena en una lista, reemplazando '0' por '.'
    sudoku = ['.' if char == '0' else char for char in cadena]
    
    # Construye las filas en el formato Prolog
    filas = [', '.join(sudoku[i:i + 9]) for i in range(0, 81, 9)]
    
    # Imprime el resultado en el formato deseado
    print(f"{nombre}([")
    for i, fila in enumerate(filas):
        if i == len(filas) - 1:  # Última fila
            print(f"    {fila}]).")
        else:
            print(f"    {fila},")


# Ejemplo de uso
cadena = "000000000560000032230040079000060000070501090000708000053000920009806500700000004"
nombre_sudoku = "sudoku7"
convertir_cadena_a_prolog(nombre_sudoku, cadena)
