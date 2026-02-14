#include <memory>
#include <atomic>

/*#include "node.hpp"  // listings/listing_6.5.cpp
template<typename T>
class queue {
    struct node {
        std::shared_ptr<T> data;
        std::unique_ptr<node> next;
    };

 std::unique_ptr<node> head;
 node* tail;
public:
 queue(): head(new node),tail(head.get()) {}
 queue(const queue& other)=delete;
 queue& operator=(const queue& other)=delete;
 std::shared_ptr<T> try_pop() {
     if(head.get()==tail)
     {
         return std::shared_ptr<T>();
     }
     std::shared_ptr<T> const res(head->data);
     std::unique_ptr<node> const old_head=std::move(head);
     head=std::move(old_head->next);
     return res;
 }
 void push(T new_value) {
     std::shared_ptr<T> new_data(
         std::make_shared<T>(std::move(new_value)));
     std::unique_ptr<node> p(new node);
     tail->data=new_data;
     node* const new_tail=p.get();
     tail->next=std::move(p);
     tail=new_tail;
 }
};

//template<typename T>
struct node {
    std::shared_ptr</ T / int *> data;
    std::unique_ptr<node> next;
};*/

/* template<typename T>  // from listing 7.16
class lock_free_queue {
    struct node;
    struct counted_node_ptr {
        int external_count;
        node* ptr;
    };
    std::atomic<counted_node_ptr> head;
    std::atomic<counted_node_ptr> tail;
    struct node_counter {
        unsigned internal_count:30;
        unsigned external_counters:2;
    };
    struct node {
        std::atomic<T*> data;
        std::atomic<node_counter> count;
        counted_node_ptr next;
        node() {
            node_counter new_count;
            new_count.internal_count=0;
            new_count.external_counters=2;
            count.store(new_count);
            next.ptr=nullptr;
            next.external_count=0;
        }
    };
public:
    void push(T new_value) {
        std::unique_ptr<T> new_data(new T(new_value));
        counted_node_ptr new_next;
        new_next.ptr=new node;
        new_next.external_count=1;
        counted_node_ptr old_tail=tail.load();
        for(;;) {
            increase_external_count(tail,old_tail);
            T* old_data=nullptr;
            if(old_tail.ptr->data.compare_exchange_strong( old_data,new_data.get())) {
                old_tail.ptr->next=new_next;
                old_tail=tail.exchange(new_next);
                free_external_counter(old_tail);
                new_data.release();
                break;
            }
            old_tail.ptr->release_ref();
        }
    }
};
struct node {  // from listing 7.16
    std::atomic<T*> data;
    std::atomic<node_counter> count;
    counted_node_ptr next;
    node() {
        node_counter new_count;
        new_count.internal_count=0;
        new_count.external_counters=2;
        count.store(new_count);
        next.ptr=nullptr;
        next.external_count=0;
    }
}; */

template<typename T>
class lock_free_queue {  // from listing 7.16
    struct node;
    struct counted_node_ptr;
    struct node_counter {
        unsigned internal_count:30;
        unsigned external_counters:2;
    };
    struct node {  // from listing 7.16
        std::atomic<T*> data;
        std::atomic<node_counter> count;
        counted_node_ptr next;
        node() {
            node_counter new_count;
            new_count.internal_count=0;
            new_count.external_counters=2;
            count.store(new_count);
            next.ptr=nullptr;
            next.external_count=0;
        }
    };
    struct counted_node_ptr
    {
        int external_count;
        node* ptr;
    };
    std::atomic<counted_node_ptr> head;
    std::atomic<counted_node_ptr> tail;
public:
    std::unique_ptr<T> pop()
    {
        counted_node_ptr old_head=head.load(std::memory_order_relaxed);
        for(;;)
        {
            increase_external_count(head,old_head);
            node* const ptr=old_head.ptr;
            if(ptr==tail.load().ptr)
            {
                ptr->release_ref();
                return std::unique_ptr<T>();
            }
            if(head.compare_exchange_strong(old_head,ptr->next))
            {
                T* const res=ptr->data.exchange(nullptr);
                free_external_counter(old_head);
                return std::unique_ptr<T>(res);
            }
            ptr->release_ref();
        }
    }
};
