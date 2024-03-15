Imports System.Collections.Generic

Module VBModule
    Public Class ExprC
    End Class

    Public Class NumC
        Inherits ExprC
        Public num As Double

        Public Sub New(ByVal number As Double)
            num = number
        End Sub
    End Class

    Public Class IdC
        Inherits ExprC
        Public id As String

        Public Sub New(ByVal identifier As String)
            id = identifier
        End Sub
    End Class

    Public Class StrC 
        Inherits ExprC
        Public str As String

        Public Sub New(ByVal str_new As String)
            str = str_new
        End Sub
    End Class
    
    Public Class IfC 
        Inherits ExprC
        Public cond As ExprC
        Public then_case As ExprC
        Public else_case As ExprC

        Public Sub New(ByVal condition As ExprC,
                        ByVal then_new As ExprC,
                        ByVal else_new As ExprC)
            cond = condition
            then_case = then_new
            else_case = else_new
        End Sub
    End Class

    Public Class LamC 
        Inherits ExprC 
        Public ids As List(Of String)
        Public body As ExprC

        Public Sub New(ByVal ids_new As List(Of String),
                        ByVal body_new As ExprC)
            ids = ids_new
            body = body_new
        End Sub
    End Class 

    Public Class AppC 
        Inherits ExprC
        Public func As ExprC 
        Public args As List(Of ExprC)

        Public Sub New(ByVal func_new As ExprC,
                        ByVal arguments As List(Of ExprC))
            func = func_new
            args = arguments
        End Sub
    End Class

    Public Class Value
    End Class

    Public Class NumV
        Inherits Value
        Public num As Double

        Public Sub New(ByVal number As Double)
            num = number
        End Sub
    End Class

    Public Class StrV
        Inherits Value
        Public str As String

        Public Sub New(ByVal str_new As String)
            str = str_new
        End Sub
    End Class

    Public Class BoolV
        Inherits Value
        Public bool As Boolean 

        Public Sub New(ByVal bool_new As Boolean)
            bool = bool_new
        End Sub
    End Class

    Public Class CloV 
        Inherits Value
        Public args As List(Of String)
        Public body As ExprC 

        Public Sub New(ByVal arguments As List(Of String),
                        ByVal body_new As ExprC)
            args = arguments
            body = body_new
        End Sub
    End Class

    Public Class PrimV
        Inherits Value
        Public op As String

        Public Sub New(ByVal operation As String)
            op = operation
        End Sub
    End Class

    Public Class Bind 
        Public name As String
        Public val As Value 

        Public Sub New(ByVal name_new As String,
                       ByVal val_new As Value)
            name = name_new
            val = val_new
        End Sub
    End Class 

    Public Class Environment
        Public bindings As List(Of Bind)

        Public Sub New()
            bindings = New List(Of Bind)()
        End Sub

        Public Sub New(ByVal bindings_new As List(Of Bind))
            bindings = bindings_new
        End Sub

        Public Sub AddBind(ByVal bind As Bind)
            bindings.Add(bind)
        End Sub
    End Class 

    Function SetTopEnv(env as Environment)
        env.AddBind(New Bind("+", New PrimV("+")))
        env.AddBind(New Bind("-", New PrimV("-")))
        env.AddBind(New Bind("*", New PrimV("*")))
        env.AddBind(New Bind("/", New PrimV("/")))
        env.AddBind(New Bind("<=", New PrimV("<=")))
        env.AddBind(New Bind("equal", New PrimV("equal")))
        env.AddBind(New Bind("error", New PrimV("error")))
        env.AddBind(New Bind("true", New BoolV(True)))
        env.AddBind(New Bind("false", New BoolV(False)))
    End Function

    Sub Main()
        Dim numObj As NumC = New NumC(10)
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)
    End Sub
End Module 
