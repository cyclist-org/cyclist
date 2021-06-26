#ifndef SLOPED_RELATION_HH_
#define SLOPED_RELATION_HH_

#include <set>
#include <map>

#include "types.c"

class Sloped_relation {

private:
    Map<int,Int_pair_SET*>* forward_map;
    Map<int,Int_pair_SET*>* backward_map;
    
public:

    Sloped_relation(void){}

    Sloped_relation(Map<int,Int_pair_SET*>* forward_map_,Map<int,Int_pair_SET*>* backward_map_)
        : forward_map(forward_map_), backward_map(backward_map_){}
    Sloped_relation( const Sloped_relation& R );
    Sloped_relation( Sloped_relation&& R );
    Sloped_relation& operator=( const Sloped_relation& R );
    Sloped_relation& operator=( Sloped_relation&& R );
    ~Sloped_relation(void);
    int size(void);
    void add( int h1, int h2, slope s );
    Int_pair_SET* get_forward_slopes( int h );
    Int_pair_SET* get_backward_slopes( int h );
    Sloped_relation compose( Sloped_relation* other );
    Sloped_relation compute_transitive_closure(void);
    void clear(void);
    friend bool operator< ( const Sloped_relation& R, const Sloped_relation& L );
    friend bool operator== ( const Sloped_relation& R, const Sloped_relation& L );
    void print_(void);
};
#endif