namespace AdventOfCode.Impl;

public abstract record Maybe<T>
{
    private Maybe() {}

    public abstract Maybe<TResult> Select<TResult>(
        Func<T, TResult> fn);

    public abstract T GetOrDefault(
        T defaultValue);

    public abstract TResult Match<TResult>(
        Func<T, TResult> some,
        Func<TResult> none);

    public abstract bool TryGet(
        out T? target);

    internal sealed record Some(T Value)
        : Maybe<T>
    {
        public override T GetOrDefault(
            T _)
            => Value;

        public override TResult Match<TResult>(
            Func<T, TResult> some,
            Func<TResult> _)
            => some(Value);

        public override Maybe<TResult> Select<TResult>(
            Func<T, TResult> fn)
            => new Maybe<TResult>.Some(fn(Value));

        public override bool TryGet(
            out T? target)
        {
            target = Value;

            return true;
        }
    }

    internal sealed record None
        : Maybe<T>
    {
        public override T GetOrDefault(
            T defaultValue)
            => defaultValue;

        public override TResult Match<TResult>(
            Func<T, TResult> _,
            Func<TResult> none)
            => none();

        public override Maybe<TResult> Select<TResult>(
            Func<T, TResult> fn)
            => new Maybe<TResult>.None();

        public override bool TryGet(
            out T? target)
        {
            target = default;

            return false;
        }
    }

    public static implicit operator Maybe<T>(
        AdventOfCode.Impl.None _)
        => new None();
}

public static class Maybe
{
    public static Maybe<T> Some<T>(
        T value)
        => new Maybe<T>.Some(value);

    public static Maybe<T> None<T>()
        => new Maybe<T>.None();

    public static None None()
        => new None();
}

public readonly struct None {}
