#include <iostream>
#include <map>
#include "freeling.h"
#include "freeling/morfo/analyzer.h"
#include "freeling/morfo/fex.h"
#include "freeling/morfo/fex_lexicon.h"
using namespace std;


///////////////////////////////////////////////////
// Load an ad-hoc set of configuration options

freeling::analyzer::config_options fill_config(const wstring &lang, const wstring &ipath) {

  freeling::analyzer::config_options cfg;

  // Language of text to process
  cfg.Lang = lang;

  // path to language specific data
  wstring lpath = ipath + L"/share/freeling/" + cfg.Lang + L"/";

  // Tokenizer configuration file
  cfg.TOK_TokenizerFile = lpath + L"tokenizer.dat";
  // Splitter configuration file
  cfg.SPLIT_SplitterFile = lpath + L"splitter.dat";
  // Morphological analyzer options
  cfg.MACO_Decimal = L".";
  cfg.MACO_Thousand = L",";
  cfg.MACO_LocutionsFile = lpath + L"locucions.dat";
  cfg.MACO_QuantitiesFile = lpath + L"quantities.dat";
  cfg.MACO_AffixFile = lpath + L"afixos.dat";
  cfg.MACO_ProbabilityFile = lpath + L"probabilitats.dat";
  cfg.MACO_DictionaryFile = lpath + L"dicc.src";
  cfg.MACO_NPDataFile = lpath + L"np.dat";
  cfg.MACO_PunctuationFile = lpath + L"../common/punct.dat";
  cfg.MACO_ProbabilityThreshold = 0.001;
  // Tagger options
  cfg.TAGGER_HMMFile = lpath + L"tagger.dat";
  cfg.TAGGER_ForceSelect = freeling::RETOK;

  // other modules are not needed in this example
  // Sense annotator and WSD config files
  cfg.SENSE_ConfigFile = L"";
  cfg.UKB_ConfigFile = L"";
  // Statistical dependency parser & SRL config file
  cfg.DEP_TreelerFile = L"";
  // NEC config file. 
  cfg.NEC_NECFile = L"";
  // Chart parser config file. 
  cfg.PARSER_GrammarFile = L"";
  // Rule based dependency parser config files. 
  cfg.DEP_TxalaFile = L"";
  // Coreference resolution config file.
  cfg.COREF_CorefFile = L"";

  return cfg;
}


///////////////////////////////////////////////////
// Load an ad-hoc set of invoke options

freeling::analyzer::invoke_options fill_invoke() {

  freeling::analyzer::invoke_options ivk;

  // Level of analysis in input and output
  ivk.InputLevel = freeling::TEXT;
  ivk.OutputLevel = freeling::MORFO; 

  // activate/deactivate morphological analyzer modules
  ivk.MACO_UserMap = false;
  ivk.MACO_AffixAnalysis = true;
  ivk.MACO_MultiwordsDetection = true;
  ivk.MACO_NumbersDetection = true;
  ivk.MACO_PunctuationDetection = true;
  ivk.MACO_DatesDetection = true;
  ivk.MACO_QuantitiesDetection  = true;
  ivk.MACO_DictionarySearch = true;
  ivk.MACO_CompoundAnalysis = false;
  ivk.MACO_NERecognition = true;
  ivk.MACO_RetokContractions = true;
  ivk.MACO_ProbabilityAssignment = true;
  ivk.TAGGER_which = freeling::HMM;

  // other modules are not used in this examples
  ivk.NEC_NEClassification = false; 
  ivk.SENSE_WSD_which = freeling::NO_WSD;
  ivk.DEP_which = freeling::NO_DEP;

  return ivk;
}

//---------------------------------------------
// Do whatever is needed with analyzed sentences
//---------------------------------------------

void ProcessSentences(list<freeling::sentence> &ls, const wstring &fname) {

  // create feature extractor and feature lexicon 
  freeling::fex fextractor(fname, L"");
  freeling::fex_lexicon lex;

  // for each sentence in list
  int ns=0;
  for (list<freeling::sentence>::iterator s=ls.begin(); s!=ls.end(); ++s, ++ns) {

    // extract features names each word in the sentence
    vector<set<wstring> > feats;
    fextractor.encode_name(*s, feats);      

    wcout << L"sentence_" << ns;
    int i=0;
    for (freeling::sentence::const_iterator w=s->begin(); w!=s->end(); ++w, ++i) {
      // print features for encoded sentence, and store them in the lexicon
      for (set<wstring>::iterator j=feats[i].begin(); j!=feats[i].end(); ++j) {
        lex.add_occurrence(*j);
        wcout<<L" "<<*j;
      }
    }
    wcout<<endl; // next sentence
  }

  // save lexicon 
  wstring lname = fname;
  lname.replace(fname.find(L".rgf"), 4, L".lex");
  lex.save_lexicon(lname, 0);
}



/////////////   MAIN PROGRAM  /////////////////////

int main (int argc, char **argv) {

  // set locale to an UTF8 compatible locale 
  freeling::util::init_locale(L"default");


  if (argc < 2) {  
    wcerr << L"Usage:   " << argv[0] << " file.rgf [language] [Freeling-dir]" << endl;
    exit (1);
  }

  // get name of feature extraction rules file.
  wstring rgfFile = freeling::util::string2wstring(argv[1]);
  // get requested language from arg1, or English if not provided
  wstring lang = L"ru";
  if (argc > 2) lang = freeling::util::string2wstring(argv[2]);
  // get installation path to use from arg2, or use /usr/local if not provided
  wstring ipath = L"/usr/local";
  if (argc > 3) ipath = freeling::util::string2wstring(argv[3]);

  // set config options (which modules to create, with which configuration)
  freeling::analyzer::config_options cfg = fill_config(lang, ipath);
  // create analyzer
  freeling::analyzer anlz(cfg);

  // set invoke options (which modules to use. Can be changed in run time)
  freeling::analyzer::invoke_options ivk = fill_invoke();
  // load invoke options into analyzer
  anlz.set_current_invoke_options(ivk);

  // load document to analyze
  wstring text;  
  wstring line;
  while (getline(wcin,line)) 
    text = text + line + L"\n";

  // analyze text, leave result in ls
  list<freeling::sentence> ls;
  anlz.analyze(text,ls);

  // extract a feature vector for each sentence
  ProcessSentences(ls, rgfFile);
}

