prices_len = int(input())
prices = [None] * prices_len
for prices_idx in range(prices_len):
    (item, price) = input().split(" ")
    price = float(price)
    prices[prices_idx] = (item, price)

orders_len = int(input())
orders = [None] * orders_len
for orders_idx in range(orders_len):
    order_name = input()
    order_len = int(input())
    order = [None] * order_len
    for order_idx in range(order_len):
        (quantity, item) = input().split(" ")
        quantity = int(quantity)
        order[order_idx] = (quantity, item)

    orders[orders_idx] = (order_name, order)