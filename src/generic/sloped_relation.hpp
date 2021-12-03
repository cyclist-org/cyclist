#ifndef SLOPED_RELATION_HH_
#define SLOPED_RELATION_HH_

#include <set>
#include <map>
#include <stack>

#include "types.c"

class Sloped_relation {

private:
    Map<int,Int_pair_SET*>* forward_map;
    Map<int,Int_pair_SET*>* backward_map;
    Map<Int_pair,int>* slope_map;

    
public:
    int max_height = -1;
    int min_height = 0;

    Sloped_relation(void){}

    Sloped_relation(Map<int,Int_pair_SET*>* forward_map_,Map<int,Int_pair_SET*>* backward_map_, Map<Int_pair,int>* slope_map_,int max_height_ = -1,int min_height_ = 0)
        : forward_map(forward_map_), backward_map(backward_map_), slope_map(slope_map_), max_height(max_height_), min_height(min_height_){}
    Sloped_relation( const Sloped_relation& R );
    Sloped_relation( Sloped_relation&& R );
    Sloped_relation& operator=( const Sloped_relation& R );
    Sloped_relation& operator=( Sloped_relation&& R );
    ~Sloped_relation(void);
    int size(void);
    void add( int h1, int h2, slope s );
    Int_pair_SET* get_forward_slopes( int h );
    Int_pair_SET* get_backward_slopes( int h );
    Map<Int_pair,int>* get_slopes(void);
    Sloped_relation* compose( Sloped_relation* other );
    Sloped_relation compute_transitive_closure(void);
    void add_or_replace(int h1, int h2, slope s);
    void clear(void);
    friend bool operator< ( const Sloped_relation& R, const Sloped_relation& L );
    friend bool operator== ( const Sloped_relation& R, const Sloped_relation& L );
    void print_(void);
    comparison compare(const Sloped_relation& lhs);

    bool has_downward_SCC(void);

};
#endif