#include <stdio.h>
typedef struct
{
    int quantity;
    char item[500];
} StructQuantityItem;
typedef struct
{
    char order_name[500];
    StructQuantityItem *order;
} StructOrder_nameOrder;
typedef struct
{
    char item[500];
    float price;
} StructItemPrice;

void print_order(StructOrder_nameOrder order)
{
    printf("%s\n", order.order_name);
    for (int i = 0; i < sizeof(order.order) / sizeof(order.order[0]); i++)
    {
        printf("%d %s\n", order.order[i].quantity, order.order[i].item);
    }
}

int main()
{

    int prices_len;
    scanf("%d", &prices_len);
    StructItemPrice prices[prices_len];
    for (int prices_idx = 0; prices_idx < prices_len; prices_idx++)
    {
        char item[500];
        float price;
        scanf("%s %f", item, &price);
        prices[prices_idx] = (StructItemPrice){item, price};
    }

    int orders_len;
    scanf("%d", &orders_len);
    StructOrder_nameOrder orders[orders_len];
    for (int orders_idx = 0; orders_idx < orders_len; orders_idx++)
    {
        char order_name[500];
        gets(order_name);
        int order_len;
        scanf("%d", &order_len);
        StructQuantityItem order[order_len];
        for (int order_idx = 0; order_idx < order_len; order_idx++)
        {
            int quantity;
            char item[500];
            scanf("%d %s", &quantity, item);
            order[order_idx] = (StructQuantityItem){quantity, *item};
        }

        orders[orders_idx] = (StructOrder_nameOrder){*order_name, order};
    }

    print_order(orders[0]);
}
