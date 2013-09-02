#ifndef PROOF_HH_
#define PROOF_HH_

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <tuple>

#include <spot/tgba/bdddict.hh>

typedef int Tag;
#define NO_TAG 0

typedef bdd Vertex;
#define NO_VERTEX bddfalse

#define HASH_VAL(s, T, v) ((s) ^= std::hash< T >()(v) + 0x9e3779b9 + ((s)<< 6) + ((s)>> 2))


typedef std::unordered_set< Tag > TagVector;
typedef std::unordered_set< Vertex > VertexSet;

//==================================================================
namespace std {
	template<> struct hash< bdd > {
		size_t operator()(bdd const &key) const { return key.id(); }
	};
}
//==================================================================
size_t hash_value(const bdd & b);
//==================================================================
//namespace std {
//    template<> struct less< bdd > {
//    	bool operator() (const bdd & b1, const bdd & b2) const {
//    		return b1.id() < b2.id();
//    	}
//    };
//}
//==================================================================
typedef std::tuple< Vertex, Vertex, Tag, Tag > Quad;
//==================================================================
namespace std {
	template<> struct hash< Quad > {
		size_t operator()(Quad const &key) const {
			int seed = 0;
			HASH_VAL(seed, Vertex, get< 0 >(key));
			HASH_VAL(seed, Vertex, get< 1 >(key));
			HASH_VAL(seed, Tag, get< 2 >(key));
			HASH_VAL(seed, Tag, get< 3 >(key));
			return seed;
		}
	};
}
//==================================================================
//namespace std {
//    template<> struct less< Quad > {
//    	bool operator() (const Quad & q1, const Quad & q2) const {
//    		if( std::less< Vertex >()(std::get<0>(q1), std::get<0>(q2)) ) return true;
//    		if( std::less< Vertex >()(std::get<0>(q2), std::get<0>(q1)) ) return false;
//
//    		if( std::less< Vertex >()(std::get<1>(q1), std::get<1>(q2)) ) return true;
//    		if( std::less< Vertex >()(std::get<1>(q2), std::get<1>(q1)) ) return false;
//
//    		if( std::less< Tag >()(std::get<2>(q1), std::get<2>(q2)) ) return true;
//    		if( std::less< Tag >()(std::get<2>(q2), std::get<2>(q1)) ) return false;
//
//    		if( std::less< Tag >()(std::get<3>(q1), std::get<3>(q2)) ) return true;
////    		if( std::less< Tag >()(std::get<3>(q2), std::get<3>(q1)) ) return false;
//
//    		return false;
//    	}
//    };
//}
//==================================================================
class Proof {
private:
	spot::bdd_dict * dict;

	size_t max_vertices_log2;
	Tag max_tag;
	int last_vertex;
	Vertex initial_vertex;

	std::vector< bdd > propositions;
	VertexSet vertices;

	typedef std::unordered_map< Vertex, VertexSet > TransitionMap;
	TransitionMap trans_map;

	typedef std::unordered_map< Vertex, TagVector > TagMap;
	TagMap tag_map;

	typedef std::unordered_map< Vertex, std::string > VertexNames;
	VertexNames vertex_names;

	std::unordered_set< Quad > trace_pairs;
	std::unordered_set< Quad > progress_pairs;

public:
	Proof(size_t maxv_l2);
	virtual ~Proof() { dict->unregister_all_my_variables(this); delete dict; }
	Vertex get_initial_vertex() const { return initial_vertex; }

	const VertexSet & get_vertices() const { return vertices; }
	const TagVector & get_tags_of_vertex(const Vertex & v) const;
	const VertexSet & get_successors(const Vertex & v) const;
	const std::string & get_vertex_name(const Vertex & v) const;
	Tag get_max_tag() const { return max_tag; }
	Vertex create_vertex();
	void tag_vertex(const Vertex & v, const Tag & t);
	void set_initial_vertex(const Vertex & v);
	void set_successor(const Vertex & v1, const Vertex & v2);

	void set_trace_pair(const Vertex & v1, const Vertex & v2,
			const Tag & t1, const Tag & t2);
	void set_progress_pair(const Vertex & v1, const Vertex & v2,
			const Tag & t1, const Tag & t2);

	virtual spot::bdd_dict* get_dict() const { return dict; }

	virtual bool trace_pair(const Vertex & v1, const Vertex & v2,
			const Tag & t1, const Tag & t2) const;
	virtual bool progress_pair(const Vertex & v1, const Vertex & v2,
			const Tag & t1, const Tag & t2) const;

};
//==================================================================

#endif /* PROOF_HH_ */
