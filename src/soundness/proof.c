#include "proof.hpp"

#include <sstream>
#include <cassert>

#include <spot/tl/defaultenv.hh>
#define ENV (spot::default_environment::instance())

//==================================================================
static bdd GET_PROP(spot::bdd_dict_ptr d, const std::string & x, const void * t) {
	const spot::formula f = ENV.require(x);
	bdd r = bdd_ithvar(d->register_proposition(f, t));
	return r;
}
//==================================================================
Proof::Proof(size_t maxv_l2) :
	dict( spot::make_bdd_dict() ),
	max_vertices_log2(maxv_l2),
	max_tag(0),
	last_vertex(0),
	initial_vertex(NO_VERTEX) {
	for(size_t i=0; i<max_vertices_log2; ++i) {
		std::stringstream ss;
		ss << "p_" << i;
		propositions.push_back( GET_PROP(dict, ss.str(), this ) );
	}
}
//------------------------------------------------------------------
const TagVector & Proof::get_tags_of_vertex(const Vertex & v) const {
	TagMap::const_iterator it = tag_map.find(v);
	assert( it != tag_map.end() );
	return it->second;
}
//------------------------------------------------------------------
const VertexSet & Proof::get_successors(const Vertex & v) const {
	TransitionMap::const_iterator tmi = trans_map.find(v);
	assert( tmi != trans_map.end() );
	return tmi->second;
}
//------------------------------------------------------------------
const std::string & Proof::get_vertex_name(const Vertex & v) const {
	VertexNames::const_iterator i = vertex_names.find(v);
	assert(i != vertex_names.end());
	return i->second;
}
//------------------------------------------------------------------
Vertex Proof::create_vertex() {
	++last_vertex;

	assert( (last_vertex>>max_vertices_log2) == 0 );

	std::stringstream ss;
	Vertex v = bddtrue;

	int l = last_vertex;
	for(size_t i=0; i<max_vertices_log2; ++i) {
		bdd b = propositions[i];
		v &= ((l % 2) ? b : bdd_not(b));
		l >>= 1;
	}

	vertices.insert(v);

	// initialise key
	tag_map[v];
	trans_map[v];

	ss.clear();
	ss << last_vertex;
	vertex_names[v] = ss.str();

//	std::cerr << "V " << v.id() << '\n';
	return v;
}
//------------------------------------------------------------------
void Proof::tag_vertex(const Vertex & v, const Tag & t) {
	assert( t>0 );
	assert( vertices.find(v) != vertices.end() );
	max_tag = std::max(max_tag, t);
	tag_map[v].insert(t);
}
//------------------------------------------------------------------
void Proof::set_initial_vertex(const Vertex & v) {
	assert( vertices.find(v) != vertices.end() );
	initial_vertex = v;
}
//------------------------------------------------------------------
void Proof::set_successor(const Vertex & v1, const Vertex & v2) {
	assert( vertices.find(v1) != vertices.end() );
	assert( vertices.find(v2) != vertices.end() );
	trans_map[v1].insert(v2);
}
//------------------------------------------------------------------
void Proof::set_trace_pair(const Vertex & v1, const Vertex & v2,
		const Tag & t1, const Tag & t2) {
	assert( vertices.find(v1) != vertices.end() );
	assert( vertices.find(v2) != vertices.end() );
	assert( tag_map[v1].find(t1) != tag_map[v1].end() );
	assert( tag_map[v2].find(t2) != tag_map[v2].end() );
	trace_pairs.insert( Quad(v1, v2, t1, t2) );
}
//------------------------------------------------------------------
void Proof::set_progress_pair(const Vertex & v1, const Vertex & v2,
		const Tag & t1, const Tag & t2) {
	assert( vertices.find(v1) != vertices.end() );
	assert( vertices.find(v2) != vertices.end() );
	assert( tag_map[v1].find(t1) != tag_map[v1].end() );
	assert( tag_map[v2].find(t2) != tag_map[v2].end() );
	progress_pairs.insert( Quad(v1, v2, t1, t2) );
}
//------------------------------------------------------------------
bool Proof::trace_pair(const Vertex & v1, const Vertex & v2,
		const Tag & t1, const Tag & t2) const {
	return trace_pairs.find( Quad(v1, v2, t1, t2) ) != trace_pairs.end();
}
//------------------------------------------------------------------
bool Proof::progress_pair(const Vertex & v1, const Vertex & v2,
		const Tag & t1, const Tag & t2) const {
	return progress_pairs.find( Quad(v1, v2, t1, t2) ) != progress_pairs.end();
}
//------------------------------------------------------------------

