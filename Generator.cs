using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Reflection;

namespace Reflection.Randomness
{
    public class FromDistribution : Attribute
    {
        public Type DistributionType { get; }
        public double[] Ranges { get; }

        public FromDistribution(Type distributionType, params double[] ranges)
        {
            DistributionType = distributionType;
            Ranges = ranges;
        }
    }

    public class Generator<T> where T : new()
    {
        private static readonly Dictionary<Type, Func<Random, T>> CachedGenerators =
            new Dictionary<Type, Func<Random, T>>();

        private static readonly Dictionary<PropertyInfo, Attribute> Attributes =
            new Dictionary<PropertyInfo, Attribute>();

        private static readonly PropertyInfo[] Properties;

        static Generator()
        {
            Properties = typeof(T).GetProperties();

            foreach (var property in Properties)
                Attributes[property] = property.GetCustomAttribute<FromDistribution>();
        }

        public T Generate(Random seed)
        {
            if (CachedGenerators.TryGetValue(typeof(T), out var generateFunc))
                return generateFunc(seed);

            var generatorExpression = GenerateExpression(seed);
            generateFunc = generatorExpression.Compile();
            CachedGenerators[typeof(T)] = generateFunc;
            return generateFunc(seed);
        }

        private Expression<Func<Random, T>> GenerateExpression(Random seed)
        {
            var newExpression = Expression.New(typeof(T));
            var memberBindings = new List<MemberBinding>();
            var parameter = Expression.Parameter(typeof(Random), "seed");

            foreach (var property in Properties)
            {
                var attribute = (FromDistribution)Attributes[property];

                if (attribute == null)
                    continue;

                Expression propertyValueExpression;

                if (attribute.DistributionType == typeof(NormalDistribution))
                    propertyValueExpression = MakeNormalExpression(attribute, parameter);
                else if (attribute.DistributionType == typeof(ExponentialDistribution))
                    propertyValueExpression = MakeExponentialExpression(attribute, parameter);
                else
                    throw new ArgumentException($"String containing {attribute.DistributionType}");

                memberBindings.Add(Expression.Bind(property, propertyValueExpression));
            }

            if (memberBindings.Count == 0)
                return Expression.Lambda<Func<Random, T>>(newExpression, parameter);

            var memberInitExpression = Expression.MemberInit(newExpression, memberBindings);
            return Expression.Lambda<Func<Random, T>>(memberInitExpression, parameter);
        }

        private static Expression MakeNormalExpression(FromDistribution attribute, Expression parameter)
        {
            if (attribute.Ranges.Length > 2)
                throw new ArgumentException($"Wrong number of arguments for {attribute.DistributionType}");

            var mean = attribute.Ranges.Length > 0 ? attribute.Ranges[0] : 0.0;
            var sigma = attribute.Ranges.Length > 1 ? attribute.Ranges[1] : 1.0;
            var normalDistribution = new NormalDistribution(mean, sigma);

            var generateMethod = typeof(NormalDistribution).GetMethod("Generate");
            var distribution = Expression.Constant(normalDistribution);

            return Expression.Call(distribution, generateMethod, parameter);
        }

        private static Expression MakeExponentialExpression(FromDistribution attribute, Expression parameter)
        {
            if (attribute.Ranges.Length != 1)
                throw new ArgumentException($"Wrong number of arguments for {attribute.DistributionType}");

            var lambda = attribute.Ranges.Length > 0 ? attribute.Ranges[0] : 1.0;
            var exponentialDistribution = new ExponentialDistribution(lambda);

            var generateMethod = typeof(ExponentialDistribution).GetMethod("Generate");
            var distribution = Expression.Constant(exponentialDistribution);

            return Expression.Call(distribution, generateMethod, parameter);
        }
    }
}