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
            bindings = New List(Of Bind)() ' Initialize the list in the constructor
        End Sub
    
        Public Sub AddBind(ByVal bind As Bind)
            bindings.Add(bind)
        End Sub
    End Class

    Sub SetTopEnv(env as Environment)
        env.AddBind(New Bind("+", New PrimV("+")))
        env.AddBind(New Bind("-", New PrimV("-")))
        env.AddBind(New Bind("*", New PrimV("*")))
        env.AddBind(New Bind("/", New PrimV("/")))
        env.AddBind(New Bind("<=", New PrimV("<=")))
        env.AddBind(New Bind("equal", New PrimV("equal")))
        env.AddBind(New Bind("error", New PrimV("error")))
        env.AddBind(New Bind("true", New BoolV(True)))
        env.AddBind(New Bind("false", New BoolV(False)))
    End Sub

    Function Serialize(val as Value) As String
        If val.GetType() = GetType(NumV) Then 
            Return "" & TryCast(val, NumV).num
        ElseIf val.GetType() = GetType(StrV) Then
            Return "" & TryCast(val, StrV).str
        ElseIf val.GetType() = GetType(BoolV) Then
            If (TryCast(val, BoolV)).bool Then
                Return "true"
            Else 
                Return "false"
            End IF
        ElseIf val.GetType() = GetType(CloV) Then
            Return "#<procedure>"
        ElseIf val.GetType() = GetType(PrimV) Then
            Return "#<primop>"
        End If
    End Function

    Function Interp(ast as ExprC, env as Environment)
    End Function
    
    Sub Main()
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)

        ' TestSetTopEnv()
        ' TestSerialize()
    End Sub

    Sub TestSetTopEnv()
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)
        
        Console.WriteLine("Top Env")
        For Each bind As Bind In topenv.bindings
            Console.WriteLine("Bind Name: " & bind.name)
            Console.WriteLine(bind.val)
        Next
    End Sub

    Sub TestSerialize() 
        Dim numval As NumV = New NumV(10)
        Dim strval As StrV = New StrV("Hello World")
        Dim trueval As BoolV = New BoolV(True)
        Dim falseval As BoolV = New BoolV(False)
        Dim listOfIds As New List(Of String)()
        listOfIds.Add("x")
        listOfIds.Add("y")
        listOfIds.Add("z")
        Dim cloval As CloV = New CloV(listOfIds, New NumC(10))
        Dim primval As PrimV = New PrimV("+")

        Console.WriteLine(Serialize(numval))
        Console.WriteLine(Serialize(strval))
        Console.WriteLine(Serialize(trueval))
        Console.WriteLine(Serialize(falseval))
        Console.WriteLine(Serialize(cloval))
        Console.WriteLine(Serialize(primval))
    End Sub

End Module 
