I'm going to solve Part 2 first, and then try to see if we can use the
mechanisms we build in order to also solve Part 1!

For part 2, the problem statement made me visualize the entire collection as a
binary tree -- a [prefix trie][trie blog], one of my favorite data structures!

[trie blog]: https://blog.jle.im/entry/tries-with-recursion-schemes.html

For example, the list:

```
001
011
101
111
```

Can be represented as:

```
           ,-[]
        ,-* 
       ,   `-[]
    ,-* 
   ,   `   ,-[]
  ,     `-* 
 ,         `-[]
*     
 `         ,-[]
  `     ,-* 
   `   ,   `-[]
    `-*
       `   ,-[]
        `-* 
           `-[]
```
