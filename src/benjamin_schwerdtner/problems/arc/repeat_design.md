# Grid Repeat Functionality Design Variations

## Context
We need to implement repeat functionality for ARC tasks where a 3x3 input pattern is repeated across a larger grid. Current output shows only the first two instances at positions (0,2) and (1,1).

## Design Variations

### 1. Pooling + Transform + Unpool

**Concept**: Use pooling to create a transformed view, apply operations, then translate back.

**Implementation sketch**:
```clojure
(defn grid-pool-repeat [grid pool-size]
  (-> grid
      (pool-into-windows pool-size pool-size)
      (map-windows (fn [window] input-pattern))
      (unpool-to-grid)))
```

**Usage**: `grid_pool_3x3 exec_s grid_unpool`

**Pros**:
- Elegant functional composition
- Reusable for different transformations
- Follows existing exec_s pattern

**Cons**:
- More complex implementation
- May need multiple instructions

### 2. Grid Context Execution (exec_grid)

**Concept**: Similar to exec_s but for grids - execute code in a new grid context.

**Implementation sketch**:
```clojure
(defn exec-grid [state window-h window-w code]
  (let [grid (stack/peek state :push/grid)
        windows (partition-grid grid window-h window-w)]
    (reduce (fn [s window]
              (-> s
                  (push-grid-context window)
                  (execute code)
                  (pop-grid-context)))
            state
            windows)))
```

**Usage**: `3 3 grid_window exec_grid (grid_copy_pattern)`

**Pros**:
- Consistent with existing exec patterns
- Clear separation of contexts
- Could support nested grid operations

**Cons**:
- Need to define grid context semantics
- More abstract for users

### 3. Simple Repeat Instruction

**Concept**: Direct repeat from top-left to bottom-right.

**Implementation sketch**:

```clojure
(defn grid-repeat-pattern [grid]
  (let [pattern (get-subgrid grid 0 0 3 3)
        non-zero-count (count-non-zero pattern)]
    (repeat-diagonal pattern non-zero-count grid)))
```

**Usage**: `grid_repeat_pattern`

**Pros**:
- Simple to understand and use
- Direct solution for the ARC task
- Minimal cognitive load

**Cons**:
- Less flexible
- Single-purpose instruction

### 4. Parameterized Repeat

**Concept**: Repeat with direction and count parameters.

**Implementation sketch**:
```clojure
(defn grid-repeat-n [grid count direction]
  (let [pattern (get-pattern grid)
        offsets (direction-offsets direction)]
    (reduce (fn [g idx]
              (place-pattern g pattern (nth offsets idx)))
            grid
            (range count))))
```

**Usage**: `4 :diagonal grid_repeat_n`

**Pros**:
- More flexible than simple repeat
- Can handle different patterns
- Still relatively simple

**Cons**:
- More parameters to manage
- Need to define direction encoding

### 5. Grid Map with Index
**Concept**: Map operation that provides grid coordinates.

**Implementation sketch**:
```clojure
(defn grid-map-indexed [grid f]
  (let [h (.height grid) w (.width grid)]
    (for [y (range h) x (range w)]
      (f x y (get-cell grid x y)))))
```

**Usage**: `grid_map_indexed (lambda [x y val] ...)`

**Pros**:
- Very flexible
- Functional programming style
- Can implement any pattern

**Cons**:
- More complex for simple cases
- Requires understanding of coordinate mapping

## Recommendation

For the immediate ARC task, I recommend implementing **Option 3 (Simple Repeat)** first as it directly solves the problem. Then, consider **Option 1 (Pooling)** or **Option 2 (exec_grid)** for a more general solution that fits with the Push3 philosophy of composable operations.

The pooling approach is particularly elegant as it treats the grid as a collection of windows that can be transformed, which aligns well with functional programming principles and could enable other interesting transformations beyond just repetition.
