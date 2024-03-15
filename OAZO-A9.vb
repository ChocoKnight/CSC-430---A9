Imports System.Collections.Generic

Module VBModule
    Public Class ExprC
    End Class

    Public Class NumC
        Implements ExprC
        Public num as Double

        Public Sub New(ByVal number As Double)
            num = number
        End Sub
    End Class

    Public Class IdC
        Implements ExprC
        Public id as String

        Public Sub New(ByVal identifier As String)
            id = identifier
        End Sub
    End Class

    Public Class StrC 
        Implements ExprC
        Public str as String

        Public Sub New(ByVal str_new As String)
            str = str_new
        End Sub
    End Class
    
    Public Class IfC 
        Implements ExprC
        Public cond as ExprC
        Public then_case as ExprC
        Public else_case as ExprC

        Public Sub New(ByVal condition As ExprC,
                        ByVal then_new As ExprC,
                        ByVal else_new As ExprC)
            cond = condition
            then_case = then_new
            else_case = else_new
        End Sub
    End Class

    Public Class LamC 
        Implements ExprC 
        Public ids as List(Of String)
        Public body as ExprC

        Public Sub New(ByVal ids_new As List(Of String),
                        ByVal body_new As ExprC)
            ids = ids_new
            body = body_new
        End Sub
    End Class 

    Public Class AppC 
        Implements ExprC
        Public func as ExprC 
        Public args as List(Of ExprC)

        Public Sub New(ByVal func_new As ExprC,
                        ByVal arguments As List(Of ExprC))
            func = func_new
            args = arguments
        End Sub
    End Class

    Public Class Value
    End Class

    Public Class NumV
        Implements Value
        Public num as Double

        Public Sub New(ByVal number As Double)
            num = number
        End Sub
    End Class

    Public Class StrV
        Implements Value
        Public str as String

        Public Sub New(ByVal str_new As String)
            str = str_new
        End Sub
    End Class

    Public Class BoolV
        Implements Value
        Public bool as Boolean 

        Public Sub New(ByVal bool_new As Boolean)
            bool = bool_new
        End Sub
    End Class

    Public Class CloV 
        Implements Value
        Public args as List(Of String)
        Public body as ExprC 

        Public Sub New(ByVal arguments As List(Of String),
                        ByVal body_new As ExprC)
            args = arguments
            body = body_new
        End Sub
    End Class

    Public Class PrimV
        Implements Value
        Public op as String

        Public Sub New(ByVal operation As String)
            op = operation
        End Sub
    End Class

    Public Class Bind 
        Public name as String
        Public val as Value 

        Public Sub New(ByVal name_new As String,
                        ByVal val_new As Value)
            name = name_new
            val = val_new
        End Sub
    End Class 

    Public Class Environment
        Public bindings as List(Of Bind)

        Public Sub New(ByVal bindings_new As List(Of Bind))
            bindings = bindings_new
        End Sub

        Public Sub AddBind(ByVal bind As Bind)
            bindings.Add(Bind)
        End Sub
    End Class 

    Dim top_env as Environment

    Sub Main()
        Dim numObj As NumC = New NumC(10)

        Console.WriteLine("NumC Object: " & numObj.num)
    End Sub
End Module 