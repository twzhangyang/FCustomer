using System;

namespace TypeShape.Core
{
    public class SubtypeWitness<TSubtype, TBase> where TSubtype : TBase
    {
        public bool IsSubtype(TBase value) => value is TSubtype;

        public TBase Upcast(TSubtype subtype) => (TBase) subtype;

        public TSubtype Downcast(TBase value) => (TSubtype)value;
    }
    
    public interface ISubtypeVisitor<in TBase, out TResult>
    {
        TResult Visit<TSubtype>() where TSubtype : TBase;
    }

    public interface ISubtypeWitnessVisitor<TBase, out TResult>
    {
        TResult Visit<TSubtype>(SubtypeWitness<TSubtype, TBase> subtypeWitness) where TSubtype : TBase;
    }

    public interface IShapeSubtype<TBase>
    {
        Type Subtype { get; }

        TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> subtypeVisitor);

        TResult Accept<TResult>(ISubtypeWitnessVisitor<TBase, TResult> subtypeWitnessVisitor);
    }

    public sealed class ShapeSubtype<TSubtype, TBase> : IShapeSubtype<TBase> where TSubtype : TBase
    {
        public Type Subtype => typeof(TSubtype);
        
        public TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> subtypeVisitor)
        {
           return subtypeVisitor.Visit<TSubtype>();
        }
        
        TResult IShapeSubtype<TBase>.Accept<TResult>(ISubtypeWitnessVisitor<TBase, TResult> subtypeWitnessVisitor)
        {
            return subtypeWitnessVisitor.Visit<TSubtype>(new SubtypeWitness<TSubtype, TBase>());
        }
    }
}