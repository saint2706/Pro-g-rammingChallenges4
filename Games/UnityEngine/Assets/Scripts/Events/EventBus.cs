using System;
using System.Collections.Generic;

namespace ChallengeEngine.Events
{
    /// <summary>
    /// Simple publish/subscribe event bus that uses strongly typed channels.
    /// Listeners are automatically removed when the owning object is disposed,
    /// which keeps editor play mode hot-reloads clean.
    /// </summary>
    public sealed class EventBus
    {
        private readonly Dictionary<Type, List<Delegate>> _subscribers = new();

        public Subscription<T> Subscribe<T>(Action<T> handler)
        {
            if (!_subscribers.TryGetValue(typeof(T), out var delegates))
            {
                delegates = new List<Delegate>();
                _subscribers[typeof(T)] = delegates;
            }

            delegates.Add(handler);
            return new Subscription<T>(this, handler);
        }

        public void Publish<T>(T payload)
        {
            if (!_subscribers.TryGetValue(typeof(T), out var delegates))
            {
                return;
            }

            foreach (var @delegate in delegates.ToArray())
            {
                if (@delegate is Action<T> action)
                {
                    action.Invoke(payload);
                }
            }
        }

        public void Clear()
        {
            _subscribers.Clear();
        }

        private void Unsubscribe<T>(Action<T> handler)
        {
            if (_subscribers.TryGetValue(typeof(T), out var delegates))
            {
                delegates.Remove(handler);
            }
        }

        public readonly struct Subscription<T> : IDisposable
        {
            private readonly EventBus _bus;
            private readonly Action<T> _handler;

            public Subscription(EventBus bus, Action<T> handler)
            {
                _bus = bus;
                _handler = handler;
            }

            public void Dispose()
            {
                _bus.Unsubscribe(_handler);
            }
        }
    }
}
