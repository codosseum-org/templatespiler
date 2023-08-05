prices_len = int(input())
prices = []
for i in range(prices_len):
    line = input()
    parts = line.split(" ")
    item = parts[0]
    price = float(parts[1])
    prices.append((item, price))

orders_len = int(input())
orders = []
for i in range(orders_len):
    name = input()

    order_len = int(input())
    order = []
    for j in range(order_len):
        line = input()
        parts = line.split(" ")
        quantity = int(parts[0])
        item = parts[1]
        order.append((quantity, item))
    
    orders.append((name, order))

print(prices, orders)