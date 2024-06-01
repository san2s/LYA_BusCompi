import difflib
import re
import sys
import matplotlib.pyplot as plt
import networkx as nx
from PyQt5.QtCore import Qt, QRect, QSize, QPoint, QRegExp
from PyQt5.QtGui import QPainter, QColor, QTextFormat, QTextCursor, QTextCharFormat, QFont
from PyQt5.QtWidgets import QApplication, QMainWindow, QWidget, QTextEdit, QLabel, QVBoxLayout, QHBoxLayout, QPushButton, QFileDialog, QTableWidget, QTableWidgetItem, QPlainTextEdit, QMessageBox
from ply import lex, yacc

def TipoValor(valor):
    tipoInt = r'\d+'
    tipoReal = r'(\d+\.\d+|\.\d+)'
    tipoStg = r'"[^"\n]*"'
    
    valor = str(valor)  # Convertir el valor a cadena para asegurar que las expresiones regulares funcionan correctamente

    if re.match(tipoReal, valor):
        return 'float'
    elif re.match(tipoInt, valor):
        return 'int'
    elif re.match(tipoStg, valor):
        return 'string'
    elif valor in ['True', 'False']:
        return 'bool'
    else:
        return None

# Función para verificar si una variable ha sido declarada antes de su uso
def verificar_variable_declarada(tabla_simbolos, identificador, numero_linea):
    if identificador not in tabla_simbolos:
        raise Exception(f"Error semántico en la línea {numero_linea}: La variable '{identificador}' no ha sido declarada.")

# Función para verificar asignaciones
def verificar_asignacion(tabla_simbolos, identificador, valor, numero_linea):
    simbolo = tabla_simbolos.get(identificador)
    if simbolo is None:
        raise Exception(f"Error semántico en la línea {numero_linea}: La variable '{identificador}' no ha sido declarada.")
    
    tipo = TipoValor(valor)
    if tipo != simbolo['type']:
        raise Exception(f"Error semántico en la línea {numero_linea}: Asignación de un valor de tipo {tipo} a una variable de tipo {simbolo['type']}.")

# Función para verificar operaciones aritméticas
def verificar_operacion(tipo1, tipo2, operador, numero_linea):
    if tipo1 != tipo2 or tipo1 not in ['int', 'float']:
        raise Exception(f"Error semántico en la línea {numero_linea}: Operación '{operador}' no soportada entre tipos {tipo1} y {tipo2}.")

# Función para verificar el número de argumentos de una función
def verificar_argumentos(identificador, argumentos, numero_linea):
    # Supongamos que 'GEO' debe tener 2 argumentos
    if identificador == 'GEO' and len(argumentos) != 2:
        raise Exception(f"Error semántico en la línea {numero_linea}: La función '{identificador}' espera 2 argumentos, pero se recibieron {len(argumentos)}.")
# Función para verificar tipos de argumentos de una función
def verificar_tipos_argumentos(identificador, argumentos, tipos_esperados, numero_linea):
    if len(argumentos) != len(tipos_esperados):
        raise Exception(f"Error semántico en la línea {numero_linea}: La función '{identificador}' espera {len(tipos_esperados)} argumentos, pero se recibieron {len(argumentos)}.")
    
    for arg, tipo_esperado in zip(argumentos, tipos_esperados):
        tipo_real = TipoValor(arg)
        if tipo_real != tipo_esperado:
            raise Exception(f"Error semántico en la línea {numero_linea}: La función '{identificador}' espera un argumento de tipo {tipo_esperado} pero se recibió {tipo_real}.")
# Función para verificar división por cero
def verificar_division_por_cero(divisor, numero_linea):
    if divisor == 0:
        raise Exception(f"Error semántico en la línea {numero_linea}: División por cero.")
# Función para verificar tipos en operaciones
def verificar_tipos_operacion(tipo1, tipo2, operador, numero_linea):
    tipos_compatibles = {
        '+': ['int', 'float', 'string'],
        '-': ['int', 'float'],
        '*': ['int', 'float'],
        '/': ['int', 'float'],
        '>': ['int', 'float'],
        '<': ['int', 'float'],
        '>=': ['int', 'float'],
        '<=': ['int', 'float'],
        '==': ['int', 'float', 'string', 'bool'],
        '!=': ['int', 'float', 'string', 'bool']
    }

    if tipo1 not in tipos_compatibles[operador] or tipo2 not in tipos_compatibles[operador]:
        raise Exception(f"Error semántico en la línea {numero_linea}: Operación '{operador}' no soportada entre tipos {tipo1} y {tipo2}.")
    if tipo1 != tipo2 and not (tipo1 in ['int', 'float'] and tipo2 in ['int', 'float']):
        raise Exception(f"Error semántico en la línea {numero_linea}: Operación '{operador}' no soportada entre tipos {tipo1} y {tipo2}.")

# Declaración de tokens
tokens = (
    'INT',
    'FLOAT',
    'STRING',
    'BOOL',
    'IDENTIFICADOR',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUALS',
    'ASSIGN',
    'EQUAL_EQUAL',
    'NOT_EQUAL',
    'LESS_THAN',
    'GREATER_THAN',
    'LESS_THAN_EQUAL',
    'GREATER_THAN_EQUAL',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'SEMICOLON',
    'COMMENT',
    'FOR',
    'WHILE',
    'IF',
    'ELSE',
    'PRINTLN',
    'MESSAGE_DIALOG',
    'SHOW',
    'LBRACKET',
    'RBRACKET',
    'COMMAS',
    'GEO',  # Agregar GEO como un token
    'POT',  # Agregar POT como un token
    'RESERVED_WORD',  # Agregar RESERVED_WORD como un token
    'NUMBER',
    'GT',
    'LT',
    'GE',
    'LE',
    'EQ',
    'NE',
)


# Expresiones regulares para tokens
t_STRING = r'"[^"\n]*"'
t_BOOL = r'True|False'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUALS = r'=='
t_ASSIGN = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'{'
t_RBRACE = r'}'
t_SEMICOLON = r';'
t_COMMENT = r'\/\/.*'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMAS = r','
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_NE = r'!='

# Ignorar espacios en blanco y saltos de línea
t_ignore = ' \t\n'

# Palabras reservadas
reserved = {
    'INT': 'INT',
    'FLOAT': 'FLOAT',
    'STRING': 'STRING',
    'BOOL': 'BOOL',
    'True': 'BOOL',
    'False': 'BOOL',
    'IF': 'IF',
    'ELSE': 'ELSE',
    'FOR': 'FOR',
    'WHILE': 'WHILE',
    'PRINTLN': 'PRINTLN',
    'MESSAGE_DIALOG': 'MESSAGE_DIALOG',
    'SHOW': 'SHOW',
    'GEO': 'GEO',
    "POT": "POT",
}

lexical_errors = []
query = ""
processed_identifiers = set()
error_output = []
error_output_set = set()
lexical_errors_set = set()
salida_output_set = set()
salida_output = []
mensaje = True
show_dialog_executed = False  # Variable para controlar si el diálogo ya se ha mostrado


# Reglas para el analizador léxico
def t_IDENTIFICADOR(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'IDENTIFICADOR')

    # Encontrar la línea y columna del token en el query
    linea = encontrar_linea_y_columna_en_query(query, t.lexpos)

    # Verificar si ya se ha procesado este identificador y si es una palabra reservada
    tvaluem = t.value
    tvalf = tvaluem.isupper() and tvaluem.isalpha()

    if tvaluem in processed_identifiers and t.value.upper() in reserved.values() and not tvalf:
        # Borrar el valor en processed_identifiers
        processed_identifiers.remove(tvaluem)
    else:
        pass

    # Verificar si el identificador no es una palabra reservada ni ha sido procesado anteriormente
    if t.value not in reserved.values() and t.value not in processed_identifiers:
        processed_identifiers.add(t.value)

        # Verificar si el identificador es una palabra reservada escrita en mayúsculas
        if t.value.isalpha() and t.value.isupper():
            # Buscar la palabra reservada más cercana
            closest_match = difflib.get_close_matches(t.value, reserved.keys(), n=1)
            if closest_match:
                closeword = closest_match[0]
                if closeword in reserved.values():
                    # Verificar si el error ya está en lexical_errors antes de agregarlo
                    error_message = f"Error léxico: '{t.value}' en la línea {linea}, quizás quisiste decir '{closeword}'"
                    if error_message not in lexical_errors:
                        lexical_errors_set.add(error_message)
        elif t.value.isalpha() and t.value.islower():
            # Buscar la palabra reservada más cercana
            closest_match = difflib.get_close_matches(t.value.upper(), reserved.keys(), n=1)
            if closest_match:
                closeword = closest_match[0]
                if closeword in reserved.values():
                    # Verificar si el error ya está en lexical_errors antes de agregarlo
                    error_message = f"Error léxico: '{t.value}' en la línea {linea}, quizás quisiste decir '{closeword}'"
                    if error_message not in lexical_errors:
                        lexical_errors_set.add(error_message)
        else:
            closest_match = difflib.get_close_matches(t.value.upper(), reserved.keys(), n=1)
            if closest_match:
                closeword = closest_match[0]
                if closeword in reserved.values():
                    # Verificar si el error ya está en lexical_errors antes de agregarlo
                    error_message = f"Error léxico: '{t.value}' en la línea {linea}, quizás quisiste decir '{closeword}'"
                    if error_message not in lexical_errors:
                        lexical_errors_set.add(error_message)

    return t


def t_NUMBER(t):
    r'\d+\.\d+|\d+'  # Expresión regular para valores decimales o enteros
    if '.' in t.value:
        t.value = float(t.value)
        t.type = f"FLOAT"  # Convertir a FLOAT si hay un punto decimal
    else:
        t.value = int(t.value)
        t.type = f"INT"  # Convertir a INT si no hay punto decimal
    return t


# Manejo de errores léxicos
def t_error(t):
    # error_msg = f"Error léxico: Token no reconocido '{t.value}' en la línea {lexer.lineno()}"
    # print(error_msg)
    # lexical_errors_set.add(error_msg)
    t.lexer.skip(1)


def t_GEO(t):
    r'GEO'
    t.type = reserved.get(t.value,
                          'GEO')  # Obtiene el tipo de token correcto desde el diccionario de palabras reservadas
    return t


def t_POT(t):
    r'POT'
    t.type = reserved.get(t.value,
                          'POT')  # Obtiene el tipo de token correcto desde el diccionario de palabras reservadas
    return t


def t_PRINTLN(t):
    r'PRINTLN'
    return t


def t_SHOW(t):
    r'SHOW'
    return t


def t_MESSAGE_DIALOG(t):
    r'MESSAGE_DIALOG'
    return t


def t_IF(t):
    r'IF'
    return t


def t_ELSE(t):
    r'ELSE'
    return t


def t_FOR(t):
    r'FOR'
    return t


def t_WHILE(t):
    r'WHILE'
    return t


lexer = lex.lex()
symbol_table = {}


def encontrar_linea_y_columna_en_query(query, lexpos):
    lineas = query.splitlines()
    contador_posicion = 0
    for numero_linea, linea in enumerate(lineas, start=1):
        if contador_posicion + len(linea) >= lexpos:
            columna = lexpos - contador_posicion
            return numero_linea
        contador_posicion += len(linea) + 1  # +1 para contar el salto de línea
    # Si la posición excede la longitud del texto, devolver la última línea
    return numero_linea


# Gramática
# Asociación de operadores y precedencia
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),  # Para el signo menos unario
)


def execute_statements(statements):
    if statements is None:
        return
    else:
        for statement in statements:
            print(f"Statement: {statement}")


def imprimir_salida(salida):
    print(f"Println de salida: {salida}")
    salida_output_set.add(str(salida))


def show_on_screen(value):
    global mensaje
    if mensaje:
        if value:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Information)
            msg.setText(f"{value}")
            msg.setInformativeText('Salida del programa')
            msg.setWindowTitle("Salida del programa")
            msg.setStandardButtons(QMessageBox.Ok | QMessageBox.Cancel)
            retval = msg.exec_()
            return
        else:
            pass


# Utility function for expression evaluation
def eval_expression(expr):
    if isinstance(expr, list):
        return [eval_expression(e) for e in expr]
    elif isinstance(expr, dict):
        # Manejo de expresiones complejas
        pass
    elif isinstance(expr, str):
        if re.match(r'^-?\d+$', expr):
            return int(expr)
        elif re.match(r'^-?\d+\.\d*$', expr):
            return float(expr)
        elif expr in {"TRUE", "FALSE"}:
            if expr == "TRUE":
                return True
            else:
                return False
        elif expr.startswith('"') and expr.endswith('"'):
            return expr[1:-1]
        elif expr in symbol_table:
            return symbol_table[expr]['value']
    return expr


def p_statements(p):
    '''
    statements : statement
               | statements SEMICOLON statement
               | statements LBRACE statements RBRACE
               | statements statement
    '''
    print(f"p_statements: {p[:]}")
    if len(p) == 2:
        p[0] = [p[1]]
        print(f"Valor de p[0] aquí (single statement): {p[0]}")
    elif len(p) == 4:
        if p[2] == ';':
            p[0] = p[1] + [p[3]]
            print(f"Valor de p[0] aquí (semicolon): {p[0]}")
        else:
            p[0] = p[1] + [p[3]]
            print(f"Valor de p[0] aquí (nested braces): {p[0]}")
    elif len(p) == 3:
        p[0] = p[1] + [p[2]]
        print(f"Valor de p[0] aquí (two statements): {p[0]}")


def p_statement(p):
    '''
    statement : assignment_after_declaration
              | variable_declaration
              | expression_assignment
              | if_statement
              | for_loop
              | while_loop
              | println_statement
              | show_statement
              | geo_statement
    '''
    print(f"p_statement: {p[:]}")
    p[0] = p[1]
    print(f"Valor de p[0] aquí: {p[0]}")


def p_geo_statement(p):
    '''
    geo_statement : GEO IDENTIFICADOR LPAREN expression_list RPAREN
    '''
    print(f"p_geo_statement: {p[:]}")

    identifier = p[2]
    params = p[4]
    term_position = p.lexpos(1)
    linea = encontrar_linea_y_columna_en_query(query, term_position)

    # Convertir los parámetros a cadenas para asegurar que las verificaciones funcionan correctamente
    params = [str(param) for param in params]

    # Verificación semántica para el número de argumentos
    try:
        verificar_argumentos(identifier, params, linea)
    except Exception as e:
        error_output_set.add(str(e))

    # Verificación semántica para los tipos de argumentos
    tipos_esperados = ['float', 'float']  # Supongamos que GEO espera dos floats
    try:
        verificar_tipos_argumentos(identifier, params, tipos_esperados, linea)
    except Exception as e:
        error_output_set.add(str(e))

    symbol_table[identifier] = {'type': 'GEO', 'params': params}
    print(f"GEO Statement: {identifier} with params {params}")


def p_assignment_after_declaration(p):
    '''
    assignment_after_declaration : IDENTIFICADOR ASSIGN expression
    '''
    print(f"p_assignment_after_declaration: {p[:]}")

    variable_name = p[1]
    term_position = p.lexpos(2)
    linea = encontrar_linea_y_columna_en_query(query, term_position)
    assigned_value = eval_expression(p[3])
    
    # Verificación semántica
    try:
        verificar_asignacion(symbol_table, variable_name, str(assigned_value), linea)
    except Exception as e:
        error_output_set.add(str(e))

    if variable_name in symbol_table:
        symbol_table[variable_name]['value'] = assigned_value
    else:
        error_output_set.add(f"Error en la línea {linea}: Variable '{variable_name}' no declarada.")


def p_variable_declaration(p):
    '''
    variable_declaration : datatype IDENTIFICADOR ASSIGN expression
                         | datatype IDENTIFICADOR
                         | datatype LBRACKET RBRACKET IDENTIFICADOR ASSIGN LBRACE expression_list RBRACE
    '''
    print(f"p_variable_declaration: {p[:]}")
    term_position = p.lexpos(2)  # Obtener la posición del término ASSIGN (puedes ajustar esto según tu necesidad)
    linea = encontrar_linea_y_columna_en_query(query, term_position)
    if len(p) == 5:  # datatype IDENTIFICADOR ASSIGN expression
        variable_type = p[1]
        variable_name = p[2]
        value = eval_expression(p[4])
        if variable_name in symbol_table:
            error_output_set.add(f"Error línea {linea}: Variable '{variable_name}' ya declarada")
        else:
            symbol_table[variable_name] = {'type': variable_type, 'value': value}
    elif len(p) == 3:  # datatype IDENTIFICADOR
        variable_type = p[1]
        variable_name = p[2]
        if variable_name in symbol_table:
            error_output_set.add(f"Error línea {linea}: Variable '{variable_name}' ya declarada")
        else:
            symbol_table[variable_name] = {'type': variable_type, 'value': None}
    elif len(p) == 9:  # datatype [] IDENTIFICADOR ASSIGN { expression_list }
        variable_type = p[1] + '[]'
        variable_name = p[4]
        values = p[7]
        if variable_name in symbol_table:
            error_output_set.add(f"Error línea {linea}: Variable '{variable_name}' ya declarada")
        else:
            symbol_table[variable_name] = {'type': variable_type, 'value': values}
            print(f"Declaración de array: {variable_name} con valores {values}")

def p_expression_list(p):
    '''
    expression_list : expression
                    | expression COMMAS expression_list
    '''
    print(f"p_expression_list: {p[:]}")

    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]



def p_expression_assignment(p):
    '''
    expression_assignment : IDENTIFICADOR ASSIGN expression
    '''
    print(f"p_expression_assignment: {p[:]}")

    variable_name = p[1]
    expression_result = eval_expression(p[3])
    term_position = p.lexpos(2)
    linea = encontrar_linea_y_columna_en_query(query, term_position)

    # Verificación semántica
    try:
        verificar_asignacion(symbol_table, variable_name, str(expression_result), linea)
    except Exception as e:
        error_output_set.add(str(e))

    if variable_name in symbol_table:
        symbol_table[variable_name]['value'] = expression_result
    else:
        error_output_set.add(f"Error en la línea {linea}: Variable '{variable_name}' no declarada.")



def p_datatype(p):
    '''
    datatype : INT
             | FLOAT
             | STRING
             | BOOL
    '''
    print(f"p_datatype: {p[:]}")
    p[0] = p[1]

def p_expression(p):
    '''
    expression : INT
               | FLOAT
               | STRING
               | IDENTIFICADOR
               | expression PLUS expression
               | expression MINUS expression
               | expression TIMES expression
               | expression DIVIDE expression
               | expression GT expression
               | expression LT expression
               | expression GE expression
               | expression LE expression
               | expression EQ expression
               | expression NE expression
               | LPAREN expression RPAREN
               | MINUS expression %prec UMINUS
    '''
    print(f"p_expression: {p[:]}")

    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        left = eval_expression(p[1])
        right = eval_expression(p[3])
        term_position = p.lexpos(2)
        linea = encontrar_linea_y_columna_en_query(query, term_position)

        # Verificación semántica para operaciones aritméticas
        try:
            verificar_tipos_operacion(TipoValor(str(left)), TipoValor(str(right)), p[2], linea)
        except Exception as e:
            error_output_set.add(str(e))

        if p[2] == '+':
            p[0] = left + right
        elif p[2] == '-':
            p[0] = left - right
        elif p[2] == '*':
            p[0] = left * right
        elif p[2] == '/':
            p[0] = left / right
        elif p[2] == '>':
            p[0] = left > right
        elif p[2] == '<':
            p[0] = left < right
        elif p[2] == '>=':
            p[0] = left >= right
        elif p[2] == '<=':
            p[0] = left <= right
        elif p[2] == '==':
            p[0] = left == right
        elif p[2] == '!=':
            p[0] = left != right
    elif len(p) == 3:
        p[0] = -eval_expression(p[2])



def p_if_statement(p):
    '''
    if_statement : IF LPAREN expression RPAREN LBRACE statements RBRACE
                 | IF LPAREN expression RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    '''
    global mensaje
    print(f"p_if_statement: {p[:]}")
    condition = eval_expression(p[3])
    print(f"Condition to eval: {condition}")

    if condition:
        print("Executing IF block")
        execute_statements(p[6])
        mensaje = False
    elif len(p) == 11:
        print("Executing ELSE block")
        execute_statements(p[10])


def p_show_statement(p):
    '''
    show_statement : SHOW MESSAGE_DIALOG LPAREN STRING RPAREN SEMICOLON
    '''
    print(f"p_show_statement: {p[:]}")

    value = eval_expression(p[4])
    print(f"Show aquí: {value}")
    show_on_screen(value)


def p_for_loop(p):
    '''
    for_loop : FOR LPAREN assignment_after_declaration SEMICOLON expression SEMICOLON expression RPAREN LBRACE statements RBRACE
    '''
    print(f"p_for_loop: {p[:]}")

    execute_statements(p[3])  # Inicialización
    while eval_expression(p[5]):  # Condición
        execute_statements(p[10])  # Cuerpo del bucle
        execute_statements(p[7])  # Incremento


def p_while_loop(p):
    '''
    while_loop : WHILE LPAREN expression RPAREN LBRACE statements RBRACE
               | WHILE LPAREN expression RPAREN statement
               | WHILE LPAREN assignment_after_declaration RPAREN SEMICOLON LBRACE statements RBRACE
    '''
    print(f"p_while_loop: {p[:]}")

    if len(p) == 8:
        while eval_expression(p[3]):
            execute_statements(p[6])
    elif len(p) == 6:
        while eval_expression(p[3]):
            execute_statements(p[5])
    else:
        execute_statements(p[3])  # Inicialización
        while eval_expression(p[5]):  # Condición
            execute_statements(p[8])  # Cuerpo del bucle


def p_println_statement(p):
    '''
    println_statement : PRINTLN LPAREN expression RPAREN SEMICOLON
                      | PRINTLN LPAREN IDENTIFICADOR RPAREN SEMICOLON
                      | PRINTLN LPAREN STRING RPAREN SEMICOLON
    '''
    print(f"p_println_statement: {p[:]}")

    if isinstance(p[3], str) and p[3] in symbol_table:
        # Obtener el identificador
        id = p[3]
        print(f"Identif : {id}")
        if 'params' in symbol_table[p[3]]:
            # Verificar si tiene parámetros
            params = symbol_table[p[3]]['params']
            if params:
                valor = f"Id: {id} - Valores: {params}"
                print(f"Println aquí: {valor}")
                imprimir_salida(valor)
            else:
                pass
        else:
            # Verificar si el identificador corresponde a un arreglo
            if isinstance(symbol_table[p[3]]['value'], list):
                value = symbol_table[p[3]]['value']
                print(f"Println de arreglo aqui: {value}")
                imprimir_salida(f"{p[3]}: {value}")
            else:
                # Evaluar la expresión si no es un identificador en la tabla de símbolos
                value = symbol_table[p[3]]['value']
                print(f"Println de valor aqui: {value}")
                imprimir_salida(value)
    else:
        # Evaluar la expresión si no es un identificador en la tabla de símbolos
        value = eval_expression(p[3])
        print(f"Println: {value}")
        imprimir_salida(value)
    p[0] = ('println_statement', p[3])


def p_error(p):
    if p:
        linea = encontrar_linea_y_columna_en_query(query, p.lexpos)

        error_output_set.add(f"Error de sintaxis en la línea {linea}: Carácter ilegal '{p.value}'")
    else:
        pass


parser = yacc.yacc()


class QLineNumberArea(QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.codeEditor = editor

    def sizeHint(self):
        return QSize(self.codeEditor.lineNumberAreaWidth(), 0)

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.fillRect(event.rect(), Qt.lightGray)
        block = self.codeEditor.firstVisibleBlock()
        blockNumber = block.blockNumber()
        height = self.codeEditor.fontMetrics().height()
        while block.isValid():
            blockGeometry = self.codeEditor.blockBoundingGeometry(block)
            top = blockGeometry.translated(self.codeEditor.contentOffset()).top()
            if top <= event.rect().bottom():  # Solo dibujar si el bloque es visible
                number = str(blockNumber + 1)
                painter.setPen(Qt.black)
                # Calcular la posición vertical basada en la altura del texto
                text_position = QPoint(0, int(top + height))
                painter.drawText(text_position, number)

            block = block.next()
            blockNumber += 1


class QCodeEditor(QPlainTextEdit):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.lineNumberArea = QLineNumberArea(self)
        self.blockCountChanged.connect(self.updateLineNumberAreaWidth)
        self.updateRequest.connect(self.updateLineNumberArea)
        self.cursorPositionChanged.connect(self.highlightCurrentLine)
        self.updateLineNumberAreaWidth(0)

        # Palabras reservadas y tokens para resaltar
        self.reserved_words = {
            'INT': 'blue',
            'FLOAT': 'cyan',
            'STRING': 'red',
            'BOOL': 'purple',
            'IDENTIFICADOR': 'darkblue',
            'PLUS': 'orange',
            'MINUS': 'cyan',
            'TIMES': 'magenta',
            'DIVIDE': 'yellow',
            'EQUALS': 'pink',
            'ASSIGN': 'brown',
            'EQUAL_EQUAL': 'gray',
            'NOT_EQUAL': 'lightblue',
            'LESS_THAN': 'lightgreen',
            'GREATER_THAN': 'lightcoral',
            'LESS_THAN_EQUAL': 'lightseagreen',
            'GREATER_THAN_EQUAL': 'lightgoldenrodyellow',
            'LPAREN': 'lightsalmon',
            'RPAREN': 'lightsteelblue',
            'LBRACE': 'lightpink',
            'RBRACE': 'lightgreen',
            'SEMICOLON': 'lightyellow',
            'COMMENT': 'gray',  # Color para los comentarios
            'FOR': 'darkred',
            'WHILE': 'darkgreen',
            'IF': 'darkblue',
            'ELSE': 'darkcyan',
            'PRINTLN': 'darkmagenta',
            'MESSAGE_DIALOG': 'darkyellow',
            'SHOW': 'darkbrown',
            'LBRACKET': 'darkgray',
            'RBRACKET': 'darkblue',
            'COMMAS': 'darkgreen',
            'GEO': 'darkred',
            'POT': 'darkorange',
            'RESERVED_WORD': 'darkviolet',
            'NUMBER': 'darkkhaki',
            'GT': 'darkgoldenrod',
            'LT': 'darkcyan',
            'GE': 'darkturquoise',
            'LE': 'darkorchid',
            'EQ': 'darkseagreen',
            'NE': 'darkslateblue'
        }

    def lineNumberAreaWidth(self):
        digits = 1
        max_value = max(1, self.blockCount())
        while max_value >= 10:
            max_value /= 10
            digits += 1
        space = 3 + self.fontMetrics().width('9') * digits
        return space

    def updateLineNumberAreaWidth(self, _):
        self.setViewportMargins(self.lineNumberAreaWidth(), 0, 0, 0)

    def updateLineNumberArea(self, rect, dy):
        if dy:
            self.lineNumberArea.scroll(0, dy)
        else:
            self.lineNumberArea.update(0, rect.y(), self.lineNumberArea.width(), rect.height())
        if rect.contains(self.viewport().rect()):
            self.updateLineNumberAreaWidth(0)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        cr = self.contentsRect()
        self.lineNumberArea.setGeometry(QRect(cr.left(), cr.top(), self.lineNumberAreaWidth(), cr.height()))

    def highlightCurrentLine(self):
        extraSelections = []
        if not self.isReadOnly():
            selection = QTextEdit.ExtraSelection()
            lineColor = QColor(Qt.transparent).lighter(160)
            selection.format.setBackground(lineColor)
            selection.format.setProperty(QTextFormat.FullWidthSelection, True)
            selection.cursor = self.textCursor()
            selection.cursor.clearSelection()
            extraSelections.append(selection)
        self.setExtraSelections(extraSelections)
        self.highlightReservedWords()  # Llamar a la función para resaltar palabras reservadas

    def highlightReservedWords(self):
        # Obtener el texto completo
        text = self.toPlainText()

        cursor = self.textCursor()
        cursor.beginEditBlock()

        # Eliminar cualquier formato previo de palabras reservadas
        cursor.select(QTextCursor.Document)
        cursor.setCharFormat(QTextCharFormat())
        cursor.clearSelection()

        # Resaltar comentarios
        comment_format = QTextCharFormat()
        comment_format.setForeground(QColor('green'))
        expression = QRegExp(r'//[^\n]*')
        index = expression.indexIn(text)
        while index >= 0:
            length = len(expression.cap(0))
            cursor.setPosition(index)
            cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor, length)
            cursor.setCharFormat(comment_format)
            index = expression.indexIn(text, index + length)

        # Resaltar palabras reservadas y tokens
        for word, color in self.reserved_words.items():
            if word != 'COMMENT':  # Ya hemos manejado los comentarios por separado
                reserved_format = QTextCharFormat()
                reserved_format.setForeground(QColor(color))

                expression = QRegExp("\\b" + re.escape(word) + "\\b")
                index = expression.indexIn(text)
                while index >= 0:
                    length = len(expression.cap(0))
                    cursor.setPosition(index)
                    cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor, length)
                    cursor.setCharFormat(reserved_format)
                    index = expression.indexIn(text, index + length)

        cursor.endEditBlock()

    def highlightLine(self, line_number, color):
        block = self.document().findBlockByLineNumber(line_number + 5)
        if not block.isValid():
            return
        cursor = QTextCursor(block)
        cursor.movePosition(QTextCursor.EndOfBlock, QTextCursor.KeepAnchor)
        char_format = QTextCharFormat()
        char_format.setBackground(QColor(color))
        char_format.setProperty(QTextFormat.FullWidthSelection, True)
        extraSelection = QTextEdit.ExtraSelection()
        extraSelection.cursor = cursor
        extraSelection.format = char_format
        extraSelections = self.extraSelections()
        extraSelections.append(extraSelection)
        self.setExtraSelections(extraSelections)

    def highlightLineN(self, line_number, color):
        print(f"Line number: {line_number}")
        block = self.document().findBlockByLineNumber(line_number)
        if not block.isValid():
            return
        cursor = QTextCursor(block)
        cursor.movePosition(QTextCursor.EndOfBlock, QTextCursor.KeepAnchor)
        char_format = QTextCharFormat()
        char_format.setBackground(QColor(color))
        char_format.setProperty(QTextFormat.FullWidthSelection, True)
        extraSelection = QTextEdit.ExtraSelection()
        extraSelection.cursor = cursor
        extraSelection.format = char_format
        extraSelections = self.extraSelections()
        extraSelections.append(extraSelection)
        self.setExtraSelections(extraSelections)

    def getTextByLineNumber(self, line_number):
        """
        Devuelve el texto de la línea especificada.

        Args:
            line_number (int): El número de la línea (0-indexed).

        Returns:
            str: El texto de la línea.
        """
        block = self.document().findBlockByLineNumber(line_number)
        if block.isValid():
            return block.text()
        else:
            return ""

    def lineNumberAreaPaintEvent(self, event):
        painter = QPainter(self.lineNumberArea)
        painter.fillRect(event.rect(), Qt.lightGray)
        block = self.firstVisibleBlock()
        blockNumber = block.blockNumber()
        top = self.blockBoundingGeometry(block).translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()
        height = self.fontMetrics().height()
        while block.isValid() and (top <= event.rect().bottom()):
            if block.isVisible() and (bottom >= event.rect().top()):
                number = str(blockNumber + 1)
                painter.setPen(Qt.black)
                painter.drawText(0, top, self.lineNumberArea.width(), height, Qt.AlignRight, number)

            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            blockNumber += 1

class MainWindow(QMainWindow):
    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)
        self.setWindowTitle("Analizador Léxico y Sintáctico")

        self.central_widget = QWidget()
        self.setCentralWidget(self.central_widget)

        layout = QHBoxLayout(self.central_widget)

        self.query_frame = QWidget()
        query_layout = QVBoxLayout(self.query_frame)

        query_label = QLabel("Querys:")
        query_layout.addWidget(query_label)

        self.query_text = QCodeEditor()
        query_layout.addWidget(self.query_text)

        # Nuevo QHBoxLayout para los botones
        button_layout_left = QVBoxLayout()
        query_layout.addLayout(button_layout_left)

        zoom_label = QLabel("Zoom:")
        button_layout_left.addWidget(zoom_label)
        # Botón de agregar (+)
        add_button = QPushButton("+")
        add_button.clicked.connect(self.agrandar_query)
        button_layout_left.addWidget(add_button)

        # Botón de eliminar (-)
        remove_button = QPushButton("-")
        remove_button.clicked.connect(self.acortar_query)
        button_layout_left.addWidget(remove_button)

        layout.addWidget(self.query_frame)

        self.syntax_frame = QWidget()
        syntax_layout = QVBoxLayout(self.syntax_frame)
        layout.addWidget(self.syntax_frame)

        syntax_label = QLabel("Sintaxis:")
        syntax_layout.addWidget(syntax_label)

        self.syntax_text = QTextEdit()
        syntax_layout.addWidget(self.syntax_text)
        self.syntax_text.setReadOnly(True)

        self.regular_exp_label = QLabel("Expresión Regular:")
        syntax_layout.addWidget(self.regular_exp_label)

        self.tabla_transiciones_label = QLabel("Tabla de Transiciones:")
        syntax_layout.addWidget(self.tabla_transiciones_label)
        self.transition_table = QTableWidget()
        self.transition_table.setColumnCount(4)
        self.transition_table.setHorizontalHeaderLabels(["Estado", "Símbolo", "Siguiente estado", "Acción"])
        syntax_layout.addWidget(self.transition_table)

        self.error_label = QLabel("Errores Sintacticos y Semanticos:")
        syntax_layout.addWidget(self.error_label)
        self.error_text = QTextEdit()
        syntax_layout.addWidget(self.error_text)
        self.error_text.setReadOnly(True)

        # Sección para mostrar la salida del programa:
        output_layout = QVBoxLayout()
        layout.addLayout(output_layout)

        self.output_label = QLabel("Salida:")
        output_layout.addWidget(self.output_label)
        self.output_text = QCodeEditor()
        output_layout.addWidget(self.output_text)
        self.output_text.setReadOnly(True)

        button_layout_right = QVBoxLayout()
        layout.addLayout(button_layout_right)

        self.boton_cargar = QPushButton("Cargar Archivo")
        self.boton_cargar.clicked.connect(self.cargar_archivo)
        button_layout_right.addWidget(self.boton_cargar)

        self.boton_analizar = QPushButton("Analizar")
        self.boton_analizar.clicked.connect(self.analizar)
        button_layout_right.addWidget(self.boton_analizar)

        self.boton_limpiar = QPushButton("Limpiar")
        self.boton_limpiar.clicked.connect(self.limpiar)
        button_layout_right.addWidget(self.boton_limpiar)

        # Agregar boton para guardar un archivo
        self.boton_guardar = QPushButton("Guardar Archivo")
        self.boton_guardar.clicked.connect(self.guardar_archivo)
        button_layout_right.addWidget(self.boton_guardar)

        self.lexical_errors_label = QLabel("Errores Léxicos:")
        syntax_layout.addWidget(self.lexical_errors_label)

        self.lexical_errors_text = QTextEdit()
        syntax_layout.addWidget(self.lexical_errors_text)
        self.lexical_errors_text.setReadOnly(True)

    def cargar_archivo(self):
        filename, _ = QFileDialog.getOpenFileName(self, "Cargar Archivo", "", "Archivos de Texto (*.txt)")
        if filename:
            with open(filename, "r") as file:
                contenido = file.read()
                self.query_text.setPlainText(contenido)
            self.boton_analizar.setEnabled(True)
            self.boton_limpiar.setEnabled(True)

    def guardar_archivo(self):
        filename, _ = QFileDialog.getSaveFileName(self, "Guardar Archivo", "", "Archivos de Texto (*.txt)")
        if filename:
            with open(filename, "w") as file:
                file.write(self.query_text.toPlainText())

    def analizar(self):
        global errores, lexical_errors, processed_identifiers, query, errores_output, lexical_errors_output, output_result, salida_output
        errores = []
        lexical_errors = []
        salida_output = []
        self.syntax_text.clear()
        self.regular_exp_label.setText("Expresión Regular:")
        self.transition_table.setRowCount(0)
        self.error_text.clear()
        self.lexical_errors_text.clear()
        self.output_text.clear()
        symbol_table.clear()
        error_output_set.clear()
        lexical_errors_set.clear()
        salida_output_set.clear()
        errores_output = ""
        lexical_errors_output = ""
        error_output.clear()
        lexical_errors.clear()
        processed_identifiers.clear()
        query = self.query_text.toPlainText()
        output = ""
        query_new = ""

        # Restablecer el número de línea del analizador léxico
        lexer.lineno = 1

        lexer.input(query)
        tokens1 = []
        for token in lexer:
            if token.value in reserved.values():
                token.type = "RESERVED_WORD"
            else:
                token.type = token.type
            tokens1.append(token)
        for token in tokens1:
            # print(f"<{token.type}> - {token.value}\n")
            output += f"<{token.type}> {token.value}\n"

        self.syntax_text.setPlainText(output)

        query_new = re.sub(r'//.*', '', query)
        if query_new.strip():
            lexer.input(query)
            self.regular_exp_label.setText(f"Expresión Regular: {t_IDENTIFICADOR.__doc__}")
            try:
                parser.parse(query_new)

            except Exception as e:
                error_output.append(f"Error de sintaxis: {e}")

            transferir_errores_lexicos()
            transferir_errores()
            transferir_salida()

            salida_output_str = [str(element) for element in salida_output]
            # Unimos los elementos en una sola cadena, separados por saltos de línea
            output_result = "\n".join(salida_output_str)

            if error_output or lexical_errors:
                if lexical_errors:
                    for error in lexical_errors:
                        linea = obtener_numero_linea(error)
                        if str(linea).isdigit():
                            if int(linea) < 10:  # Convierte la línea de nuevo a un entero para compararla
                                self.query_text.highlightLineN(int(linea) - 1, Qt.red)
                            else:
                                self.query_text.highlightLine(int(linea), Qt.red)
                        else:
                            pass

                    # Mostrar errores léxicos en el widget de texto
                    lexical_error_output = "\n".join(lexical_errors)
                    self.lexical_errors_text.setPlainText(lexical_error_output)

                    return
                if error_output:
                    for error in error_output:
                        linea = obtener_numero_linea(error)
                        if str(linea).isdigit():
                            if int(linea) < 10:  # Convierte la línea de nuevo a un entero para compararla
                                self.query_text.highlightLineN(int(linea) - 1, Qt.red)
                            else:
                                self.query_text.highlightLine(int(linea), Qt.red)
                        else:
                            pass

                    errores_output = "\n".join(error_output)
                    self.error_text.setPlainText(errores_output)

                    return
            else:
                self.mostrar_tabla_transiciones()
                self.output_text.setPlainText(output_result)

    def mostrar_tabla_transiciones(self):
        # Define los headers de la tabla
        headers = ["Estado", "Símbolo", "Siguiente estado", "Acción"]
        self.transition_table.setColumnCount(len(headers))
        self.transition_table.setHorizontalHeaderLabels(headers)

        tabla_transiciones = [
            ["E0", "letra", "E1", "Iniciar identificador"],
            ["E0", "dígito", "E4", "Iniciar número entero"],
            ["E0", ".", "E5", "Iniciar número real"],
            ["E0", '"', "E3", "Iniciar cadena"],
            ["E0", "/", "E2", "Iniciar comentario"],
            ["E0", "otro", "E6", "Error"],
            ["E1", "letra", "E1", "Continuar identificador"],
            ["E1", "dígito", "E1", "Continuar identificador"],
            ["E1", "_", "E1", "Continuar identificador"],
            ["E1", "otro", "E6", "Error"],
            ["E2", "/", "E2", "Continuar comentario"],
            ["E2", "\n", "E0", "Fin comentario"],
            ["E2", "otro", "E2", "Continuar comentario"],
            ["E3", '"', "E0", "Fin cadena"],
            ["E3", "otro", "E3", "Continuar cadena"],
            ["E4", "dígito", "E4", "Continuar número entero"],
            ["E4", ".", "E5", "Convertir a número real"],
            ["E4", "otro", "E6", "Error"],
            ["E5", "dígito", "E5", "Continuar número real"],
            ["E5", "otro", "E6", "Error"],
            ["E6", "-", "-", "Ignorar"]
        ]

        self.transition_table.setRowCount(len(tabla_transiciones))
        for i, fila in enumerate(tabla_transiciones):
            for j, valor in enumerate(fila):
                item = QTableWidgetItem(valor)
                self.transition_table.setItem(i, j, item)

        generar_y_mostrar_automata(tabla_transiciones)

    def limpiar(self):
        self.query_text.clear()
        self.syntax_text.clear()
        self.regular_exp_label.setText("Expresión Regular:")
        self.transition_table.setRowCount(0)
        self.error_text.clear()
        self.lexical_errors_text.clear()
        self.output_text.clear()

    # Función para agrandar el textbox de query presionando el boton +
    def agrandar_query(self):
        current_font = self.query_text.font()
        current_size = current_font.pointSize()
        new_size = current_size + 1  # Puedes ajustar el incremento según prefieras
        current_font.setPointSize(new_size)
        self.query_text.setFont(current_font)

        # agrandar el texto tambien del cuadro de syntax, errores y salida
        self.syntax_text.setFont(current_font)
        self.output_text.setFont(current_font)
        self.lexical_errors_text.setFont(current_font)
        self.error_text.setFont(current_font)

    def acortar_query(self):
        current_font = self.query_text.font()
        current_size = current_font.pointSize()
        new_size = current_size - 1
        current_font.setPointSize(new_size)
        self.query_text.setFont(current_font)
        self.syntax_text.setFont(current_font)
        self.output_text.setFont(current_font)
        self.lexical_errors_text.setFont(current_font)
        self.error_text.setFont(current_font)


def generar_y_mostrar_automata(tabla_transiciones):
    G = nx.DiGraph()

    for fila in tabla_transiciones:
        origen = fila[0]
        simbolo = fila[1]
        destino = fila[2]
        if G.has_edge(origen, destino):
            G[origen][destino]['symbols'].append(simbolo)
        else:
            G.add_edge(origen, destino, symbols=[simbolo])

    plt.figure(figsize=(10, 8))
    pos = nx.spring_layout(G)
    nx.draw(G, pos, with_labels=True, node_size=1500, node_color="skyblue", font_size=12, font_weight="bold",
            arrows=True)

    edge_labels = {}
    for origen, destino, data in G.edges(data=True):
        symbols = data['symbols']
        edge_labels[(origen, destino)] = '/'.join(symbols)

    nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)

    plt.title("Autómata de Pila para la tabla de transiciones")
    plt.show()


def obtener_numero_linea(cadena):
    # Expresión regular para encontrar el número después de "línea"
    coincidencia = re.search(r'línea (\d+)', cadena)
    if coincidencia:
        return int(coincidencia.group(1))
    return float('inf')  # Devolver infinito si no se encuentra el número de línea para que vayan al final


def transferir_errores_lexicos():
    global lexical_errors, lexical_errors_set
    # Ordenar lexical_errors_set por el número de línea
    sorted_lexical_errors = sorted(lexical_errors_set, key=obtener_numero_linea)
    lexical_errors.extend(sorted_lexical_errors)


def transferir_errores():
    global error_output, error_output_set
    # Ordenar error_output_set por el número de línea
    sorted_errors = sorted(error_output_set, key=obtener_numero_linea)
    error_output.extend(sorted_errors)


def transferir_salida():
    global salida_output, salida_output_set
    salida_output.extend(salida_output_set)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec_())
