namespace TypeProviderPlayground

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

module TypeHelpers =
    let inline tmethod n = typeof<Type>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.NonPublic)
    let inline invoke (t: Type) (m: MethodInfo) p = m.Invoke(t, p) |> unbox
    let IsByRefImpl = tmethod "IsByRefImpl"
    let IsArrayImpl = tmethod "IsArrayImpl"
    let GetAttributeFlagsImpl = tmethod "GetAttributeFlagsImpl"
    let IsPointerImpl = tmethod "IsPointerImpl"
    let GetNestedType = tmethod "GetNestedType"
    let IsPrimitiveImpl = tmethod "IsPrimitiveImpl"
    let GetPropertyImpl = tmethod "GetPropertyImpl"
    let IsCOMObjectImpl = tmethod "IsCOMObjectImpl"
    let HasElementTypeImpl = tmethod "HasElementTypeImpl"
    let GetMethodImpl = tmethod "GetMethodImpl"
    let GetConstructorImpl = tmethod "GetConstructorImpl"

    type DelegatingType(t: Type) =
        inherit Type()
        let invoke x = invoke t x
        override x.IsByRefImpl() = invoke IsByRefImpl [||]
        override x.IsArrayImpl() = invoke IsArrayImpl [||]
        override x.GetAttributeFlagsImpl() = invoke GetAttributeFlagsImpl [||]
        override x.GetMembers bindingAttr = t.GetMembers bindingAttr
        override x.IsPointerImpl() = invoke IsPointerImpl [||]
        override x.GetNestedType(name, bindingAttr) = invoke GetNestedType [|name;bindingAttr|]
        override x.GetNestedTypes bindingAttr = t.GetNestedTypes bindingAttr
        override x.GetProperties bindingAttr = t.GetProperties bindingAttr
        override x.IsPrimitiveImpl() = invoke IsPrimitiveImpl [||]
        override x.GetPropertyImpl(name, bindingAttr, binder, returnType, types, modifiers) = invoke GetPropertyImpl [|name; bindingAttr; binder; returnType; types; modifiers|]
        override x.GetEvents bindingAttr = t.GetEvents bindingAttr
        override x.GetEvent(name, bindingAttr) = t.GetEvent(name, bindingAttr)
        override x.IsCOMObjectImpl() = invoke IsCOMObjectImpl [||]
        override x.GetInterfaces() = t.GetInterfaces()
        override x.GetElementType() = t.GetElementType()
        override x.GetInterface(name, ignoreCase) = t.GetInterface(name, ignoreCase)
        override x.HasElementTypeImpl() = invoke HasElementTypeImpl [||]
        override x.GetFields bindingAttr = t.GetFields bindingAttr
        override x.GetField(name, bindingAttr) = t.GetField(name, bindingAttr)
        override x.GetMethods bindingAttr = t.GetMethods bindingAttr
        override x.UnderlyingSystemType = t.UnderlyingSystemType
        override x.GetMethodImpl(name, bindingAttr, binder, callConvention, types, modifiers) = invoke GetMethodImpl [|name; bindingAttr; binder; callConvention; types; modifiers|]
        override x.BaseType = t.BaseType
        override x.Name = t.Name
        override x.AssemblyQualifiedName = t.AssemblyQualifiedName
        override x.Namespace = t.Namespace
        override x.FullName = t.FullName
        override x.Assembly = t.Assembly
        override x.Module = t.Module
        override x.GUID = t.GUID
        override x.GetConstructors bindingAttr = t.GetConstructors bindingAttr
        override x.GetConstructorImpl(bindingFlags, binder, callConvention, types, modifiers) = invoke GetConstructorImpl [|bindingFlags; binder; callConvention; types; modifiers|]
        override x.GetCustomAttributes(attributeType, ainherit) = t.GetCustomAttributes(attributeType, ainherit)
        override x.GetCustomAttributes ainherit = t.GetCustomAttributes ainherit
        override x.IsDefined(attributeType, ainherit) = t.IsDefined(attributeType, ainherit)
        override x.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters) = t.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters)
        override x.GetCustomAttributesData() = upcast ResizeArray<_>()

    type DelegatingMethodInfo(m: MethodInfo) =
        inherit MethodInfo()
        override x.GetBaseDefinition() = m.GetBaseDefinition()
        override x.GetCustomAttributes ainherit = m.GetCustomAttributes ainherit
        override x.GetCustomAttributes(attributeType, ainherit) = m.GetCustomAttributes(attributeType, ainherit)
        override x.IsDefined(attributeType, ainherit) = m.IsDefined(attributeType, ainherit)
        override x.ReflectedType = m.ReflectedType
        override x.DeclaringType = m.DeclaringType
        override x.Name = m.Name
        override x.Invoke(obj, invokeAttr, binder, parameters, culture) = m.Invoke(obj, invokeAttr, binder, parameters, culture)
        override x.Attributes = m.Attributes
        override x.ReturnTypeCustomAttributes = m.ReturnTypeCustomAttributes
        override x.GetParameters() = m.GetParameters()
        override x.GetMethodImplementationFlags() = m.GetMethodImplementationFlags()
        override x.MethodHandle = m.MethodHandle

    type DelegatingMemberInfo(m: MemberInfo) =
        inherit MemberInfo()
        override x.GetCustomAttributes ainherit = m.GetCustomAttributes ainherit
        override x.GetCustomAttributes(attributeType, ainherit) = m.GetCustomAttributes(attributeType, ainherit)
        override x.IsDefined(attributeType, ainherit) = m.IsDefined(attributeType, ainherit)
        override x.ReflectedType = m.ReflectedType
        override x.DeclaringType = m.DeclaringType
        override x.Name = m.Name
        override x.MemberType = m.MemberType


    let erase t : Type = 
        let erased : TypeAttributes = enum (int TypeProviderTypeAttributes.IsErased)
        upcast { new DelegatingType(t) with
                    override x.GetAttributeFlagsImpl() = (invoke t GetAttributeFlagsImpl [||]) ||| erased }

    let rename n t : Type = 
        let declMethod t m : MethodInfo = 
            upcast { new DelegatingMethodInfo(m) with
                        override mx.DeclaringType = t }
        let declMember t m : MemberInfo = 
            upcast { new DelegatingMemberInfo(m) with
                        override mx.DeclaringType = t }
        upcast { new DelegatingType(t) with
                    override x.Name = n 
                    override x.GetMethods bindingAttr = 
                        t.GetMethods bindingAttr 
                        |> Array.map (declMethod x)
                    override x.GetMembers bindingAttr =
                        t.GetMembers bindingAttr
                        |> Array.map (declMember x) }
            