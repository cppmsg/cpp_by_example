namespace Messaging {
    class Receiver {
        Queue q;
    public:
        operator sender() { return sender(&q); }
        dispatcher wait() { return dispatcher(&q); }
    };
}
