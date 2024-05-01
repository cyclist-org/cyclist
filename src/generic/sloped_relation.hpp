#ifndef SLOPED_RELATION_HH_
#define SLOPED_RELATION_HH_

#include "types.c"

class Sloped_relation {

private:
    Map<int,Int_pair_SET*>* forward_map = NULL;        
    Map<int,Int_pair_SET*>* backward_map = NULL;
    Map<Int_pair,int>* slope_map = NULL;
    
    // A canonical matrix representing the relation
    int** repr_matrix = NULL;

    // Dimensions of the array representation
    int num_src_heights = 0;
    int num_dst_heights = 0;

    // Pointer to boolean indicating whether relation viewed as a graph has a
    // strongly connected component containing a down slope.
    // Use this pointer to avoid recomputing this
    bool* has_down_scc = NULL;

    std::mutex initialize_mutex;

    Sloped_relation(Map<int,Int_pair_SET*>* forward_map_,
                    Map<int,Int_pair_SET*>* backward_map_,
                    Map<Int_pair,int>* slope_map_,
                    int num_src_heights_,
                    int num_dst_heights_)
        : forward_map(forward_map_),
          backward_map(backward_map_),
          slope_map(slope_map_),
          num_src_heights(num_src_heights_),
          num_dst_heights(num_dst_heights_)
        {}

public:


    Sloped_relation(int num_src_heights, int num_dst_heights)
        : Sloped_relation(num_src_heights, num_dst_heights, Undef) {}
    Sloped_relation(int num_src_heights, int num_dst_heights, slope diagonal);

    // This constructor used in compute_transitive_closure
    Sloped_relation( const Sloped_relation& R );

    ~Sloped_relation(void);
    
    // Reset the object to represent the empty relation
    void reset(void);

    void initialize(void);
    int size(void) const;

    // Requires h1 < num_src_heights, h2 < num_dst_heights
    bool add (int h1, int h2, slope s);
    void remove (int h1, int h2, slope s);

    slope get_slope(int src_h, int dst_h);

    // Computes the composition of [left] and [right]
    // The result is computed and stored in-place, in the receiver
    // The relations being composed must be initialised
    // The receiver must have been constructed with sufficient heights
    // If the receiver is not initialised, then it remains so
    // The receiver needs to represent the empty relation to guarantee that the
    // result is correct
    void compose(Sloped_relation& left, Sloped_relation& right);

    // Computes the composition of [left] and [right]
    // The result is computed and stored in a new object to which a pointer is returned
    // The receiver and the argument must both be initialised
    // The resulting object is also initialised
    Sloped_relation* compose( Sloped_relation& other );
    
    Sloped_relation* compute_transitive_closure(void);

    // Computes the partial ordering between the receiver and the argument
    comparison compare(const Sloped_relation& other);
    
    bool has_self_loop(void);
    bool has_downward_SCC(void);
    bool has_downward_slope();

    friend bool operator== ( const Sloped_relation& R, const Sloped_relation& L );

    struct linear_order {
        bool operator() (const Sloped_relation& lhs, const Sloped_relation& rhs) const;
    };

    const Map<int,Int_pair_SET*>* get_forward_map();
    const Map<Int_pair,int>* get_slope_map();
    Int_pair_SET* get_height_neighbours(int src_height);
    
    void print_(void);
    friend std::ostream& operator<<(std::ostream& os, const Sloped_relation& r);

};
#endif