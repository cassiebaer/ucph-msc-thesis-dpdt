{-
The purpose of this document is to demonstrate a direct translation from PINQ's
types into Idris.
It also is a reference for the "complete" PINQ API.
-}

Epsilon : Type
Epsilon = Double
data IEnumerable a = MkIEnumerable
data IQueryable a = MkIQueryable
data PINQueryable a = MkPINQueryable
data IGrouping a b = MkIGrouping
data Expr a = MkExpr


-- double NoisyCount(double epsilon)
-- double NoisyAverage(double epsilon, Expression<Func<T, double>> function)
-- double NoisySum(double epsilon, Expression<Func<T, double>> function)
-- double NoisyOrderStatistic(double epsilon, double fraction, Expression<Func<T, double>> function)
-- double NoisyMedian(double epsilon, Expression<Func<T, double>> function)
-- R ExponentialMechanism<R>(double epsilon, IQueryable<R> range, Expression<Func<T, R, double>> scoreFunc)
noisyCount : PINQueryable a -> Epsilon -> Double
noisyAverage : PINQueryable a -> Epsilon -> Expr (a -> Double) -> Double
noisySum : PINQueryable a -> Epsilon -> Expr (a -> Double) -> Double
noisyOrderStatistic : PINQueryable a -> Epsilon -> Double -> Expr (a -> Double) -> Double
noisyMedian : PINQueryable a -> Epsilon -> Expr (a -> Double) -> Double
exponentialMechanism : PINQueryable a -> Epsilon -> IQueryable b -> Expr (a -> b -> Double) -> b


-- PINQueryable<T> Where(Expression<Func<T, bool>> predicate)
-- PINQueryable<S> Select<S>(Expression<Func<T, S>> selector)
-- PINQueryable<S> SelectMany<S>(int k, Expression<Func<T, IEnumerable<S>>> selector)
where' : PINQueryable a -> Expr (a -> Bool) -> PINQueryable a
select : PINQueryable a -> Expr (a -> b) -> PINQueryable b
selectMany : PINQueryable a -> Integer -> Expr (a -> IEnumerable b) -> PINQueryable b



-- PINQueryable<T> Take(int count)
-- PINQueryable<T> Skip(int count)
take : PINQueryable a -> Integer -> PINQueryable a
skip : PINQueryable a -> Integer -> PINQueryable a


--PINQueryable<R> Join<S, K, R>(IQueryable<S> other,                       Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IGrouping<K, S>, R>> resultSelector)
--PINQueryable<R> Join<S, K, R>(IQueryable<S> other, PINQAgent otherAgent, Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IGrouping<K, S>, R>> resultSelector)
--PINQueryable<R> Join<S, K, R>(PINQueryable<S> other,                     Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IGrouping<K, S>, R>> resultSelector)
--PINQueryable<R> Join<S, K, R>(IQueryable<S> other, PINQAgent otherAgent, Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, int bound1, int bound2, Expression<Func<T, S, R>> resultSelector)
--PINQueryable<R> Join<S, K, R>(IQueryable<S> other,                       Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, int bound1, int bound2, Expression<Func<T, S, R>> resultSelector)
--PINQueryable<R> Join<S, K, R>(PINQueryable<S> other,                     Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, int bound1, int bound2, Expression<Func<T, S, R>> resultSelector)
join : PINQueryable a -> PINQueryable b -> Expr (a -> k) -> Expr (b -> k) -> Expr (IGrouping k a -> IGrouping k b -> r) -> PINQueryable r
--PINQueryable<R> GroupJoin<S, K, R>(IQueryable<S> other, Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<T,               IEnumerable<S>, R>> resultSelector)
--PINQueryable<R> GroupJoin<S, K, R>(IQueryable<S> other, Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IEnumerable<S>, R>> resultSelector)
--PINQueryable<R> GroupJoin<S, K, R>(IQueryable<S> other, PINQAgent otherAgent, Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IEnumerable<S>, R>> resultSelector)
--PINQueryable<R> GroupJoin<S, K, R>(PINQueryable<S> other,                     Expression<Func<T, K>> keySelector1, Expression<Func<S, K>> keySelector2, Expression<Func<IGrouping<K, T>, IEnumerable<S>, R>> resultSelector)
groupJoin : PINQueryable a -> PINQueryable b -> Expr (a -> k) -> Expr (b -> k) -> Expr (IGrouping k a -> IEnumerable b -> r) -> PINQueryable r
--PINQueryable<T> Concat(IQueryable<T> other)
--PINQueryable<T> Concat(IQueryable<T> other, PINQAgent otherAgent)
--PINQueryable<T> Concat(PINQueryable<T> other)
concat : PINQueryable a -> PINQueryable a -> PINQueryable a


--PINQueryable<IGrouping<K, T>> GroupBy<K>(Expression<Func<T, K>> keySelector)
groupBy : PINQueryable a -> Expr (a -> k) -> PINQueryable (IGrouping k a)


--PINQueryable<T> Distinct()
--PINQueryable<T> Distinct(int k)
--PINQueryable<T> Distinct<K>(int k, Expression<Func<T, K>> keySelector)
distinct : PINQueryable a -> Integer -> Expr (a -> k) -> PINQueryable a
--PINQueryable<T> Union(IQueryable<T> other)
--PINQueryable<T> Union(IQueryable<T> other, PINQAgent otherAgent)
--PINQueryable<T> Union(PINQueryable<T> other)
union : PINQueryable a -> PINQueryable a -> PINQueryable a
--PINQueryable<T> Intersect(IQueryable<T> other)
--PINQueryable<T> Intersect(IQueryable<T> other, PINQAgent otherAgent)
--PINQueryable<T> Intersect(PINQueryable<T> other)
intersect : PINQueryable a -> PINQueryable a -> PINQueryable a
--PINQueryable<T> Except(IQueryable<T> other)
--PINQueryable<T> Except(IQueryable<T> other, PINQAgent otherAgent)
--PINQueryable<T> Except(PINQueryable<T> other)
except : PINQueryable a -> PINQueryable a -> PINQueryable a

