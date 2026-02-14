namespace Messaging {
    class receiver {
        queue q;
    public:
        operator sender() { return sender(&q); }
        dispatcher wait() { return dispatcher(&q); }
    };
}
