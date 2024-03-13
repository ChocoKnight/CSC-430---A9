Imports System.Collections.Generic

Module VBModule

    Public Enum ExprCType
        NumC
        IdC
        StrC
        IfC
        LamC
        AppC
    End Enum

    ' Define ExprC Class
    Public Class ExprC
        Public Type As ExprCType
        Public Num As Double?
        Public Id As String
        Public Str As String
        Public Condition As ExprC
        Public ThenExpr As ExprC
        Public ElseExpr As ExprC
        Public Ids As List(Of String)
        Public Body As ExprC
        Public Func As ExprC
        Public Arguments As List(Of ExprC)
    End Class

    Public Enum ValueType
        NumV
        StrV
        CloV
        BoolV
        PrimV
    End Enum

    ' Define Value Class
    Public Class Value
        Public Typ As ValueType 'Type is a keyword
        Public Num As Double?
        Public Str As String
        Public Args As List(Of String)
        Public Body As ExprC
        Public Env As Environment
        Public Bool As Boolean?
        Public Op As String
    End Class

    ' Define Environment Class
    Public Class Environment
        Public Bindings As List(Of Bind)
    End Class

    ' Define Bind Class
    Public Class Bind
        Public Name As String
        Public Val As Value
    End Class

    ' Top Level Environment
    ' Public Class TopEnv
    '     Public Shared ReadOnly Bindings As List(Of Bind) = New List(Of Bind) From {
    '         New Bind With {.Name = "+", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "+"}},
    '         New Bind With {.Name = "-", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "-"}},
    '         New Bind With {.Name = "*", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "*"}},
    '         New Bind With {.Name = "/", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "/"}},
    '         New Bind With {.Name = "<=", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "<="}},
    '         New Bind With {.Name = "equal?", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "equal?"}},
    '         New Bind With {.Name = "error", .Val = New Value With {.Typ = ValueType.PrimV, .Op = "error"}},
    '         New Bind With {.Name = "true", .Val = New Value With {.Typ = ValueType.BoolV, .Bool = True}},
    '         New Bind With {.Name = "false", .Val = New Value With {.Typ = ValueType.BoolV, .Bool = False}}
    '     }
    ' End Class
    'error in TopEnv

    Sub Main()
        Dim expr As New ExprC()

        ' Set properties of the ExprC instance
        expr.Id = "123"
        expr.Str = "Hello from ExprC!"

        ' Print out the properties of the ExprC instance
        Console.WriteLine("Id: " & expr.Id)
        Console.WriteLine("Str: " & expr.Str)
    End Sub
End Module
