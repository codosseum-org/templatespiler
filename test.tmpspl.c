#include <stdio.h>

typedef struct
{
    char *name;
    int price;
} Item;

typedef struct
{
    int quantity;
    char *item;
} Order;

int main()
{
    int pricesLen;
    scanf("%d", &pricesLen);
    Item prices[pricesLen];

    for (int i = 0; i < pricesLen; i++)
    {
        char item[100];
        float price = 0;
        scanf("%s %f", item, &price);
        prices[i] = (Item){item, price};
    }

    int ordersLen;
    scanf("%d", &ordersLen);

    char *orders[ordersLen];

    for (int i = 0; i < ordersLen; i++)
    {
        char name[100];
        scanf("%s\n", name);

        int orderLen;
        scanf("%d", &orderLen);
        Order order[orderLen];

        for (int j = 0; j < orderLen; j++)
        {
            int quantity;
            char item[100];
            scanf("%d %s", &quantity, item);
            order[j] = (Order){quantity, item};
        }
    }
    printf("Done\n");
}