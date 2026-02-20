namespace Messaging {
    class Sender {
        Queue * q;
    public:
        Sender(): q(nullptr) {}
        explicit Sender(Queue * q_): q(q_) {}
        template<typename Message>
        void send(Message const& msg) {
            if(q) { q->push(msg); }
        }
    };
}
