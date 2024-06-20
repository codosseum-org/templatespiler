#include <stdio.h>

int main() {
    typedef struct {
        int quantity;
        char* item;
    } StructQuantityItem;
    typedef struct {
        char* order_name;
        StructQuantityItem* order;
    } StructOrder_nameOrder;
    typedef struct {
        char* item;
        float price;
    } StructItemPrice;
    int prices_len;
    scanf("%d", prices_len);
    StructItemPrice prices[prices_len];
    for (int prices_idx = 0 ; prices_idx < prices_len ; prices_idx++) {
        char* item;
        float price;
        scanf("%s %f", item,  price);
        prices[prices_idx] = (StructItemPrice){item, price};
    }

    int orders_len;
    scanf("%d", orders_len);
    StructOrder_nameOrder orders[orders_len];
    for (int orders_idx = 0 ; orders_idx < orders_len ; orders_idx++) {
        char* order_name;
        gets(order_name);
        int order_len;
        scanf("%d", order_len);
        StructQuantityItem order[order_len];
        for (int order_idx = 0 ; order_idx < order_len ; order_idx++) {
            int quantity;
            char* item;
            scanf("%d %s", quantity,  item);
            order[order_idx] = (StructQuantityItem){quantity, item};
        }

        orders[orders_idx] = (StructOrder_nameOrder){order_name, order};
    }

}