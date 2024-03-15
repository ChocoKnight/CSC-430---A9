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
        Public env as Environment

        Public Sub New(ByVal arguments As List(Of String),
                        ByVal body_new As ExprC,
                        ByVal env_new As Environment)
            args = arguments
            body = body_new
            env = env_new
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
        If TypeOf val Is NumV Then 
            Return "" & TryCast(val, NumV).num
        ElseIf TypeOf val Is StrV Then
            Return "" & TryCast(val, StrV).str
        ElseIf TypeOf val Is BoolV Then
            If (TryCast(val, BoolV)).bool Then
                Return "true"
            Else 
                Return "false"
            End If
        ElseIf TypeOf val Is CloV Then
            Return "#<procedure>"
        ElseIf TypeOf val Is PrimV Then
            Return "#<primop>"
        End If
    End Function

    Function Interp(ast as ExprC, env as Environment) As Value
        If TypeOf ast Is NumC Then 
            Return New NumV(DirectCast(ast, NumC).num)
        ElseIf TypeOf ast Is IdC Then
            Return Lookup(DirectCast(ast, IdC).id, env)
        ElseIf TypeOf ast Is StrC Then
            Return New StrV(DirectCast(ast, StrC).str)
        ElseIf TypeOf ast Is IfC Then
            Dim if_statement As IfC = DirectCast(ast, IfC)
            Dim if_cond As Value = Interp(if_statement.cond, env)
            If TypeOf if_cond Is BoolV Then
                If DirectCast(if_cond, BoolV).bool Then
                    Return Interp(if_statement.then_case, env)
                Else 
                    Return Interp(if_statement.else_case, env)
                End If
            Else
                Throw New System.Exception("Interp - Condition was not a boolean")
            End If
        ElseIf TypeOf ast Is LamC Then
            Dim anon As LamC = TryCast(ast, LamC)
            Return New CloV(anon.ids, anon.body, env)
        ElseIf TypeOf ast Is AppC Then
            Dim func As Value = Interp(ast, env)
            If TypeOf func Is CloV Then 
                Throw New System.Exception("OAZO Interp - Invalid AppC")
            ElseIf TypeOf func Is PrimV Then
                Prim_Op_Function(DirectCast(func, PrimV).op, DirectCast(ast, AppC).args, env)
            Else 
                Throw New System.Exception("OAZO Interp - Invalid AppC")
            End If
        End If
    End Function

    Function Lookup(id As String, env as Environment) As Value
        For Each bind As Bind In env.bindings
            If bind.name = id Then 
                Return bind.val
            End If
        Next
        Throw New System.Exception("OAZO Lookup - Variable not found in environment")
    End Function

    Function Prim_Op_Function(op As String, args As List(Of ExprC), env As Environment) As Value
        If args.Count = 2 Then
            Dim left As NumV = Interp(args(0), env)
            Dim right As NumV = Interp(args(1), env)

            If op = "+" Then 
                Return New NumV(left.num + right.num)
            ElseIf op = "-" Then
                Return New NumV(left.num - right.num)
            ElseIf op = "*" Then
                Return New NumV(left.num * right.num)
            ElseIf op = "/" Then
                If right.num = 0 Then 
                    Throw New System.Exception("OAZO Prim-Op-Func Divide By Zero")
                Else 
                    Return New NumV(left.num / right.num)
                End If
            ElseIf op = "<=" Then
                Return New BoolV(left.num <= right.num)
            ElseIf op = "equal?" Then
                Return New BoolV(left.num = right.num)
            Else 
                Throw New System.Exception("OAZO Prim-Op-Func Two Args ")
            End If
        ElseIf args.Count = 1 Then 
            If op = "error" Then 
                Throw New System.Exception("OAZO Error " & Serialize(Interp(args(0), env)))
            Else 
                Throw New System.Exception("OAZO Prim-Op-Func error ")
            End If
        Else 
            If op = "true" Then 
                Return New BoolV(True)
            ElseIf op = "false" Then 
                Return New BoolV(False)
            Else 
                Throw New System.Exception("OAZO Prim-Op-Func No Args No Match")
            End If
        End If
    End Function
    
    Sub Main()
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)

        ' TestSetTopEnv()
        ' TestSerialize()
        ' TestInterp()
        
        Dim addition As AppC = New AppC(New IdC("+"), New List(Of ExprC)({New NumC(5), New NumC(5)}))
        ' Dim additionVariable As AppC = New AppC(New LamC(New List(Of String)({"x", "y"}), New AppC(New IdC("+"), New List(Of ExprC)({New IdC("x"), New IdC("y")}))), New List(Of ExprC)({New NumC(5), New IdC(5)}))
        ' Console.WriteLine(Serialize(Interp(addition, topenv)))
    End Sub

    Sub TestInterp()
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)

        Dim num_expr As NumC = New NumC(10)
        Console.WriteLine(Serialize(Interp(num_expr, topenv)))

        Dim id_expr As IdC = New IdC("+")
        Console.WriteLine(Serialize(Interp(id_expr, topenv)))

        Dim str_expr As StrC = New StrC("Hello World")
        Console.WriteLine(Serialize(Interp(str_expr, topenv)))
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
        Dim topenv As Environment = New Environment()
        SetTopEnv(topenv)

        Dim numval As NumV = New NumV(10)
        Dim strval As StrV = New StrV("Hello World")
        Dim trueval As BoolV = New BoolV(True)
        Dim falseval As BoolV = New BoolV(False)
        Dim listOfIds As New List(Of String)()
        listOfIds.Add("x")
        listOfIds.Add("y")
        listOfIds.Add("z")
        Dim cloval As CloV = New CloV(listOfIds, New NumC(10), topenv)
        Dim primval As PrimV = New PrimV("+")

        Console.WriteLine(Serialize(numval))
        Console.WriteLine(Serialize(strval))
        Console.WriteLine(Serialize(trueval))
        Console.WriteLine(Serialize(falseval))
        Console.WriteLine(Serialize(cloval))
        Console.WriteLine(Serialize(primval))
    End Sub

End Module 
