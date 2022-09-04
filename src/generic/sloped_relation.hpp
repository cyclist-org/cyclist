#ifndef SLOPED_RELATION_HH_
#define SLOPED_RELATION_HH_

#include <unordered_set>
#include <set>
#include <map>
#include <stack>
#include <ostream>
#include <vector>

#include "types.c"

class Sloped_relation {

private:
    Map<int,Int_pair_SET*>* forward_map;        
    Map<int,Int_pair_SET*>* backward_map;
    Map<Int_pair,int>* slope_map;
    
    // Flag for checking if relation has been initialized
    bool initialized = false;
    
    // A canonical matrix representing the relation
    // int** repr_matrix;

    // Dimensions of the array representation
    int num_src_heights = 0;
    int num_dst_heights = 0;

    Sloped_relation(Map<int,Int_pair_SET*>* forward_map_,
                    Map<int,Int_pair_SET*>* backward_map_,
                    Map<Int_pair,int>* slope_map_,
                    int num_src_heights_,
                    int num_dst_heights_)
        : forward_map(forward_map_),
          backward_map(backward_map_),
          slope_map(slope_map_),
          num_src_heights(num_src_heights_),
          num_dst_heights(num_dst_heights_),
          initialized(true)
        {}

public:
    // A canonical matrix representing the relation
    int** repr_matrix = 0;

    Sloped_relation(void){}
    Sloped_relation(int num_src_heights, int num_dst_heights);
    Sloped_relation( const Sloped_relation& R );
    Sloped_relation( Sloped_relation&& R );
    Sloped_relation& operator=( const Sloped_relation& R );
    Sloped_relation& operator=( Sloped_relation&& R );
    ~Sloped_relation(void);
    void clear(void);

    void initialize(void);
    int size(void) const;

    // Requires h1 < num_src_heights, h2 < num_dst_heights
    void add( int h1, int h2, slope s );
    
    Int_pair_SET* get_forward_slopes( int h );
    Int_pair_SET* get_backward_slopes( int h );
    Map<Int_pair,int>* get_slopes(void);

    Sloped_relation* compose( Sloped_relation& other );
    Sloped_relation* compute_transitive_closure(void);
    comparison compare(const Sloped_relation& lhs);
    bool has_downward_SCC(void);

    friend bool operator< ( const Sloped_relation& R, const Sloped_relation& L );
    friend bool operator== ( const Sloped_relation& R, const Sloped_relation& L );
    
    void print_(void);
    friend std::ostream& operator<<(std::ostream& os, const Sloped_relation& r);

};
#endif