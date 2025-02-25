//! Sequence Alignment elements
//!
//! Adapted from ["seqalign.asn"](https://www.ncbi.nlm.nih.gov/IEB/ToolBox/CPP_DOC/lxr/source/src/objects/seqalign/seqalign.asn)

use crate::general::{ObjectId, UserObject};
use crate::seqloc::{NaStrand, SeqId, SeqLoc};
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

pub type SeqAlignSet = Vec<SeqAlign>;

#[derive(Clone, Serialize_repr, Deserialize_repr, PartialEq, Debug)]
#[repr(u8)]
/// Internal representation of alignment type for [`SeqAlign`]
///
/// # Note
///
/// Original implementation lists this as `ENUMERATED`, therefore it is assumed that
/// serialized representation is an integer
pub enum SeqAlignType {
    NotSet,

    Global,
    /// unbroken, but not ordered, diagonals
    Diags,
    /// mapping pieces together
    Partial,
    /// discontinuous alignment
    Disc,

    Other = 255,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
pub enum SeqAlignSegs {
    DenDiag(Vec<DenseDiag>),
    DenSeg(DenseSeg),
    Std(Vec<StdSeg>),
    Packed(PackedSeg),
    Disc(SeqAlignSet),
    Spliced(SplicedSeg),
    Sparse(SparseSeg),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct SeqAlign {
    #[serde(rename = "type")]
    pub r#type: SeqAlignType,
    /// dimensionality
    pub dim: Option<u64>,
    /// for whole alignment
    pub score: Option<Vec<Score>>,
    /// alignment data
    pub segs: SeqAlignSegs,
    /// regions of sequence over which
    /// alignment was computed
    pub bounds: Option<Vec<SeqLoc>>,
    /// alignment id
    pub id: Option<Vec<ObjectId>>,
    /// extra info
    pub ext: Option<Vec<UserObject>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// for (multiway) diagonals
pub struct DenseDiag {
    /// dimensionality
    // TODO: default 2
    pub dim: u64,
    /// sequences in order
    pub ids: Vec<SeqId>,
    /// start OFFSETS in ids order
    pub starts: Vec<u64>,
    /// len of aligned segments
    pub len: u64,
    pub strands: Option<Vec<NaStrand>>,
    pub scores: Option<Vec<Score>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// The densest packing for sequence alignments only.
///
/// # Description
///
/// A start of -1 indicates a gap for that sequence of length lens.
///
///  id=100  AAGGCCTTTTAGAGATGATGATGATGATGA
///  id=200  AAGGCCTTTTAG.......GATGATGATGA
///  id=300  ....CCTTTTAGAGATGATGAT....ATGA
///
///  dim = 3, numseg = 6, ids = { 100, 200, 300 }
///  starts = { 0,0,-1, 4,4,0, 12,-1,8, 19,12,15, 22,15,-1, 26,19,18 }
///  lens = { 4, 8, 7, 3, 4, 4 }
///
pub struct DenseSeg {
    // TODO: default 2
    /// dimensionality
    pub dim: u64,
    /// number of segments here
    pub numseg: u64,
    /// sequences in order
    pub ids: Vec<SeqId>,
    /// start OFFSETS in ids order within segs
    pub starts: Vec<u64>,
    /// lengths in ids order within segs
    pub lens: Vec<u64>,
    pub strands: Option<Vec<NaStrand>>,
    /// score for each seg
    pub scores: Option<Vec<Score>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// for (multiway) global or partial alignments
pub struct PackedSeg {
    // TODO: default 2
    /// dimensionality
    pub dim: u64,

    /// number of segments here
    pub numseg: u64,

    /// sequences in order
    pub ids: Vec<SeqId>,

    /// start OFFSETS in ids order for whole alignment
    pub starts: Vec<u64>,

    /// Boolean if each sequence present or absent in each segment
    pub present: Vec<u8>,

    /// length of each segment
    pub lens: Vec<u64>,

    pub strands: Option<Vec<NaStrand>>,

    /// score for each segment
    pub scores: Option<Vec<Score>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct StdSeg {
    // TODO: default 2
    /// dimensionality
    pub dim: u64,

    /// sequences in order
    pub ids: Option<Vec<SeqId>>,

    pub loc: Vec<SeqLoc>,

    /// score for each segment
    pub scores: Option<Vec<Score>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
pub enum SplicedSegProduct {
    Transcript,
    Protein,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct SplicedSeg {
    /// product is either protein or transcript (cDNA)
    pub product_id: Option<SeqId>,
    pub genomic_id: Option<SeqId>,

    /// should be 'plus' or 'minus'
    pub product_strand: Option<NaStrand>,
    pub genomic_strand: Option<NaStrand>,

    pub product_type: SplicedSegProduct,

    /// set of segments involved each segment corresponds
    /// to one exon.
    ///
    /// Exons are always in biological order.
    pub exons: Vec<SplicedExon>,

    /// start of poly(A) tail on the transcript
    /// For sense transcripts:
    ///     `aligned product positions` < `poly_a` <= `product_length`
    ///     `poly_a == product_length`
    ///     indicates inferred poly(A) tail at transcript's end
    /// For anti-sense transcripts:
    ///     -1 <= `poly_a` < `align product positions`
    ///     `poly_a == -1`
    ///     indicates inferred poly(a) tail at transcript's start
    pub poly_a: Option<i64>,

    /// length of the product, in bases/residues
    ///
    /// from this (or from poly_a, if present),
    /// a 3' aligned length can be extracted
    pub product_length: Option<u64>,

    /// alignment descriptors / modifiers
    ///
    /// this provides a set for extension
    pub modifiers: Option<Vec<SplicedSegModifier>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum SplicedSegModifier {
    /// start found for protein/product or genomic alignment
    StartCodonFound(bool),

    /// stop found for protein/product or genomic alignment
    StopCodonFound(bool),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
/// Complete or partial exon
///
/// Two consecutive [`SplicedExon`]'s may belong to one exon
pub struct SplicedExon {
    /// `product_end >= product_start`
    pub product_start: ProductPos,
    pub product_end: ProductPos,

    /// `genomic_end >= genomic_start`
    pub genomic_start: i64,
    pub genomic_end: i64,

    /// product is either protein or transcript (cDNA)
    pub product_id: Option<SeqId>,
    pub genomic_id: Option<SeqId>,

    /// should be 'plus' or 'minus'
    pub product_strand: Option<NaStrand>,

    /// represents the strand of translation
    pub genomic_strand: Option<NaStrand>,

    /// basic segments always are in biologic order
    pub parts: Option<Vec<SplicedExonChunk>>,

    /// scores for this exon
    pub scores: Option<ScoreSet>,

    /// splice sites
    pub acceptor_before_exon: Option<SpliceSite>,
    pub donor_after_exon: Option<SpliceSite>,

    /// flag: is this exon complete or partial?
    pub partial: Option<bool>,

    /// extra info
    pub ext: Option<Vec<UserObject>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ProductPos {
    NucPos(u64),
    ProtPos(ProtPos),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// codon based position on protein (1/3 of aminoacid)
pub struct ProtPos {
    /// standard protein position
    pub amin: u64,

    /// 0, 1, 2, or 3 as for [`CdRegion`]
    /// 0 = not set
    /// 1, 2, 3 = actual frame
    pub frame: usize,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
/// Piece of an exon
///
/// Each variant contains lengths given in nucleotide bases
/// (1/3 of amino acid when product is a protein)
pub enum SplicedExonChunk {
    /// both sequences represented, product and genomic sequences match
    Match(u64),

    /// both sequences represented, product and genomic sequences do not match
    Mismatch(u64),

    /// both sequences are represented, there is sufficient similarity
    /// between product and genomic sequences. Can be used to replace
    /// stretches of matches and mismatches, mostly for protein to genomic
    /// where definition of match or mismatch depends on translation table
    Diag(u64),

    /// Insertion in product sequence
    /// (ie: gap in the genomic sequence)
    ProductIns(u64),

    /// Insertion in product sequence
    /// (ie: gap in the genomic sequence)
    GenomicIns(u64),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// site involved in splice
pub struct SpliceSite {
    /// typically two bases in the introgenic region,
    /// always in IUPAC format
    pub bases: String,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
/// [`SparseSeg`] follows the semantics of [`DenseSeg`] and is optimized
/// for representing sparse multiple alignments.
pub struct SparseSeg {
    pub master_id: Option<SeqId>,
    /// pairwise alignments constituting this multiple alignment
    pub rows: Vec<SparseAlign>,

    /// per-row scores
    pub row_scores: Option<Vec<Score>>,

    /// index of extra items
    pub ext: Option<Vec<SparseSegExt>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct SparseAlign {
    pub first_id: SeqId,
    pub second_id: SeqId,

    /// number of segments
    pub numseg: u64,

    /// starts on the first sequence (`numseg`)
    pub first_starts: Vec<u64>,

    /// starts on the second sequence (`numseg`)
    pub second_starts: Vec<u64>,

    /// lengths of segments (`numseg`)
    pub lens: Vec<u64>,

    pub second_strands: Option<Vec<NaStrand>>,

    /// per-segment scores
    pub seg_scores: Option<Vec<Score>>,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct SparseSegExt {
    pub index: u64,
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ScoreValue {
    Real(f64),
    Int(i64),
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
/// Use of [`Score`] is discouraged for external ASN.1 specifications
pub struct Score {
    pub id: Option<ObjectId>,
    pub value: ScoreValue,
}

pub type ScoreSet = Vec<Score>;
