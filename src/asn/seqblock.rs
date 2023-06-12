//! EMBL specific data
//!
//! Adapted from ["seqblock.asn"](https://www.ncbi.nlm.nih.gov/IEB/ToolBox/CPP_DOC/lxr/source/src/objects/seqblock/seqblock.asn)
//! from the NCBI C++ Toolkit.

use crate::general::{Date, DbTag, ObjectId};
use crate::seqloc::{SeqId};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum EMBLDbNameCode {
    EMBL,
    GenBank,
    DDBJ,
    GenInfo,
    MedLine,
    SWISSPROT,
    PIR,
    PDB,
    EPD,
    ECD,
    TFD,
    FlyBase,
    ProSite,
    Enzyme,
    MIM,
    EcoSeq,
    HIV,
    Other = 255,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum EMBLDbName {
    Code(EMBLDbNameCode),
    Name(String),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct EMBLXref {
    pub db_name: EMBLDbName,
    pub id: Vec<ObjectId>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Default)]
pub enum EMBLBlockClass {
    NotSet,
    #[default]
    Standard,
    Unannotated,
    Other = 255,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum EMBLBlockDiv {
    Fun,
    Inv,
    Mam,
    Org,
    Pln,
    Pri,
    Pro,
    Rod,
    Syn,
    Una,
    Vrl,
    Vrt,
    Pat,
    Est,
    STS,
    Other = 255,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct EMBLBlock {
    pub class: EMBLBlockClass,
    pub div: EMBLBlockDiv,
    pub creation_date: Date,
    pub update_date: Date,
    pub extra_acc: Option<Vec<String>>,
    pub keywords: Option<Vec<String>>,
    pub xref: Option<Vec<EMBLXref>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
/// internal representation of `class` field for [`SPBlock`]
pub enum SPBlockClass {
    NotSet,
    /// conforms to all SWISSPROT checks
    Standard,
    /// only seq and biblio checked
    Prelim,
    Other = 255,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
/// SWISSPROT specific descriptions
pub struct SPBlock {
    pub class: SPBlockClass,

    /// old SWISSPROT id's
    pub extra_acc: Option<Vec<String>>,

    /// seq known to start with Met
    /// Should default to false
    pub imeth: bool,

    /// plasmid names carrying gene
    pub plasnm: Option<Vec<String>>,

    /// xref to other sequences
    pub seqref: Option<Vec<SeqId>>,

    /// xref to non-sequence db's
    pub dbref: Option<Vec<DbTag>>,

    /// keywords
    pub keywords: Option<Vec<String>>,

    /// creation date
    pub created: Option<Date>,

    /// sequence update
    pub sequpd: Option<Date>,

    /// annotation update
    pub annotupd: Option<Date>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
/// PIR specific descriptions
pub struct PIRBlock {
    /// had punctuation in sequence?
    pub had_punct: Option<bool>,

    pub host: Option<String>,

    /// source line
    pub source: Option<String>,

    pub summary: Option<String>,
    pub genetic: Option<String>,
    pub includes: Option<String>,
    pub placement: Option<String>,
    pub superfamily: Option<String>,
    pub keywords: Option<Vec<String>>,
    pub cross_reference: Option<String>,
    pub date: Option<String>,

    /// seq with punctuation
    pub seq_raw: Option<String>,

    /// xref to other sequences
    pub seqref: Option<Vec<SeqId>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct GBBlock {
    pub extra_accessions: Option<Vec<String>>,
    /// source line
    pub source: Option<String>,
    pub keywords: Option<Vec<String>>,
    pub origin: Option<String>,

    /// *OBSOLETE* old form entry date
    pub date: Option<String>,

    /// replaces date
    pub entry_date: Option<Date>,

    /// GenBank division
    pub div: Option<String>,

    /// continuation line of organism
    pub taxonomy: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
/// Protein Research Foundation specific definition
pub struct PRFBlock {
    pub extra_src: Option<PRFExtraSrc>,
    pub keywords: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct PRFExtraSrc {
    pub host: Option<String>,
    pub part: Option<String>,
    pub state: Option<String>,
    pub strain: Option<String>,
    pub taxon: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
/// PDB specific descriptions
pub struct PDBBlock {
    /// deposition date: month,year
    pub deposition: Date,

    pub class: String,
    pub compound: Vec<String>,
    pub source: Vec<String>,

    /// present if NOT X-ray diffraction
    pub exp_method: Option<String>,

    /// replacement history
    pub replace: Option<PDBReplace>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct PDBReplace {
    pub date: Date,

    /// entry ids replace by this one
    pub ids: Vec<String>,
}
