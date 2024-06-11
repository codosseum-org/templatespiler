#include <stdio.h>

int main()
{
    int prices_len;
    scanf("%d", prices_len);
    struct
    {
        char *item;
        float price;
    } prices[prices_len];
    for (int prices_idx = 0; prices_idx < prices_len; prices_idx++)
    {
        char *item;
        float price;
        scanf("%s %f", item, price);
        prices[prices_idx] = (struct {
            char *item;
            float price;
        }){item, price};
    }

    int orders_len;
    scanf("%d", orders_len);
    struct
    {
        char *order_name;
        struct
        {
            int quantity;
            char *item;
        } *order;
    } orders[orders_len];
    for (int orders_idx = 0; orders_idx < orders_len; orders_idx++)
    {
        char *order_name;
        gets(order_name);
        int order_len;
        scanf("%d", order_len);
        struct
        {
            int quantity;
            char *item;
        } order[order_len];
        for (int order_idx = 0; order_idx < order_len; order_idx++)
        {
            int quantity;
            char *item;
            scanf("%d %s", quantity, item);
            order[order_idx] = {quantity, item};
        }

        orders[orders_idx] = {order_name, order};
    }
}