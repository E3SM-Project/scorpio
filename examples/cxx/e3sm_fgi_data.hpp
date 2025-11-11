#ifndef __E3SM_FGI_DATA_HPP__
#define __E3SM_FGI_DATA_HPP__

#include "e3sm_fgi_utils.hpp"
#include <type_traits>
#include <numeric>
#include <functional>
#include <stdexcept>
#include <typeinfo>
#include <cassert>
#include <map>
#include <memory>

namespace E3SM_FGI{
  class SPIO_file;

  class IHas_name_id{
    public:
      virtual std::string name(void ) const = 0;
      virtual int id(void ) const = 0;
      virtual ~IHas_name_id() = default;
  };

  class SPIO_file_obj : public IHas_name_id{
    public:
      virtual int def(SPIO_file &f) = 0;
      virtual int put(void ) = 0;
      virtual int get_and_verify(void ) const = 0;
      virtual ~SPIO_file_obj() = default;
  };

  class SPIO_valid_file : public IHas_name_id{
    public:
      virtual ~SPIO_valid_file() = default;
  };
  class SPIO_valid_dim : public SPIO_file_obj{
    public:
      virtual ~SPIO_valid_dim() = default;
  };
  class SPIO_valid_var : public SPIO_file_obj{
    public:
      virtual ~SPIO_valid_var() = default;
  };

  template<typename T>
  class SPIO_var;
  class SPIO_dim;

  class SPIO_decomp : public IHas_name_id{
    public:
      SPIO_decomp(const std::string &name, int iosysid, int pio_type,
                  const std::vector<int> &gdim_sz,
                  int lsz, std::function<PIO_Offset(void)> &gen):
        name_(name), iosysid_(iosysid), pio_type_(pio_type),
        gdim_sz_(gdim_sz), lsz_(lsz), id_(INVALID_ID)
      {
        int ret = PIO_NOERR;
        assert((iosysid >= 0) && gen);
        std::vector<PIO_Offset> decomp_map;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Creating decomp : " + name_ + "\n");

        decomp_map.reserve(lsz);
        std::generate_n(std::back_inserter(decomp_map), lsz, gen);

        ret = PIOc_InitDecomp(iosysid, pio_type, gdim_sz.size(), gdim_sz.data(),
                lsz, decomp_map.data(), &id_, NULL, NULL, NULL);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_InitDecomp() failed : ");
          err_msg += "iosysid = " + std::to_string(iosysid)
            + ", pio_type = " + std::to_string(pio_type)
            + ", gdim_sz = " + Util::String::vec_to_string(gdim_sz)
            + ", lsz = " + std::to_string(lsz) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          throw std::runtime_error(err_msg);
        }
      }

      SPIO_decomp(const SPIO_decomp &other) = delete;
      SPIO_decomp &operator=(const SPIO_decomp &other) = delete;
      SPIO_decomp(SPIO_decomp &&other) :
        name_(std::move(other.name_)), iosysid_(other.iosysid_), pio_type_(other.pio_type_),
        gdim_sz_(std::move(other.gdim_sz_)), lsz_(other.lsz_), id_(other.id_)
      {
        /* Invalidate other */
        other.id_ = INVALID_ID;
      }

      std::string name(void ) const { return name_; }
      int id(void ) const { return id_; }

      ~SPIO_decomp()
      {
        int ret = PIO_NOERR;
        if(has_valid_decomp()){
          Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Freeing decomp : " + name_ + "\n");
          ret = PIOc_freedecomp(iosysid_, id_);
          if(ret != PIO_NOERR){
            std::string err_msg("PIOc_freedecomp() failed :");
            err_msg += "iosysid = " + std::to_string(iosysid_)
              + ", pio_type = " + std::to_string(pio_type_)
              + ", gdim_sz = " + Util::String::vec_to_string(gdim_sz_)
              + ", lsz = " + std::to_string(lsz_) + "\n";

            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          }
        }
      }
    private:
      static const int INVALID_ID = -1;

      const std::string name_;
      int iosysid_;
      int pio_type_;
      std::vector<int> gdim_sz_;
      int lsz_;
      int id_;

      bool has_valid_decomp(void ) const { return id_ > 0; }
  };

  namespace Type_Traits{
    template<typename T>
    struct cxx_to_pio_type : std::false_type {};

    template<>
    struct cxx_to_pio_type<char> { static constexpr int pio_type = PIO_CHAR; };

    template<>
    struct cxx_to_pio_type<std::string> { static constexpr int pio_type = PIO_CHAR; };

    template<>
    struct cxx_to_pio_type<int> { static constexpr int pio_type = PIO_INT; };

    template<>
    struct cxx_to_pio_type<float> { static constexpr int pio_type = PIO_FLOAT; };

    template<>
    struct cxx_to_pio_type<double> { static constexpr int pio_type = PIO_DOUBLE; };

  } // namespace TypeTraits

  class SPIO_file : public SPIO_valid_file{
    public:
      SPIO_file(int iosysid, const std::string &name, int iotype) : iosysid_(iosysid),
          name_(name), iotype_(iotype), fh_(INVALID_ID), is_in_def_mode_(false),
          del_on_close_(true){
        int ret = PIO_NOERR;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Creating file : " + name_ + "\n");
        ret = create_file(iosysid, name, iotype);
        if(ret == PIO_NOERR){
          is_in_def_mode_ = true;
        }
        else{
          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Creating file : " + name + "failed\n");
        }
      }

      SPIO_file(int iosysid, const std::string &name, int iotype, bool del_on_close) :
          iosysid_(iosysid), name_(name), iotype_(iotype), fh_(INVALID_ID),
          is_in_def_mode_(false), del_on_close_(del_on_close){
        int ret = PIO_NOERR;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Creating file : " + name_ + "\n");
        ret = create_file(iosysid, name, iotype);
        if(ret == PIO_NOERR){
          is_in_def_mode_ = true;
        }
        else{
          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Creating file : " + name + "failed\n");
        }
      }

      std::string name(void ) const { return name_; }
      int id(void ) const { return fh_; }
      int fh(void ) const { return id(); }

      int def(const std::vector<std::shared_ptr<SPIO_file_obj> > &fobjs){
        int ret = PIO_NOERR;
        assert(fh_ != INVALID_ID);
        if(!is_in_def_mode_){
          ret = PIOc_redef(fh_);
          if(ret != PIO_NOERR){
            std::string err_msg("PIOc_redef(");
            err_msg += "file = " + name_
                      + ") failed, ret = " + std::to_string(ret) + "\n";
            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
            return ret;
          }
        }

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Defining file objects in file : " + name_ + "\n");
        for(std::vector<std::shared_ptr<SPIO_file_obj> >::const_iterator citer = fobjs.cbegin();
            citer != fobjs.cend(); ++citer){
          ret = (*citer)->def(*this);
          if(ret != PIO_NOERR){
            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Defining file object failed\n");
            return ret;
          }

          dcache_[(*citer)->name()] = *citer;
        }

        ret = PIOc_enddef(fh_);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_enddef(");
          err_msg += "file = " + name_
                    + ") failed, ret = " + std::to_string(ret) + "\n";
          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        is_in_def_mode_ = false;
       
        return PIO_NOERR;
      }

      int put(void ){
        int ret = PIO_NOERR;
        assert(fh_ != INVALID_ID);
        assert(!is_in_def_mode_);

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Writing file objects in file : " + name_ + "\n");
        for(std::map<std::string, std::shared_ptr<SPIO_file_obj> >::const_iterator citer = dcache_.cbegin();
            citer != dcache_.cend(); ++citer){
          ret = citer->second->put();
          if(ret != PIO_NOERR){
            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Put/write of variable failed\n");
            return ret;
          }
        }
        return PIO_NOERR;
      }

      std::shared_ptr<const SPIO_file_obj> get(const std::string &name) const{
        return dcache_.at(name);
      }

      int get_and_verify(void ) { return PIO_NOERR; }

      ~SPIO_file(){
        if(fh_ != INVALID_ID){
          PIOc_closefile(fh_);
        }
        if(del_on_close_ && !name_.empty()){
          PIOc_deletefile(iosysid_, name_.c_str());
        }
      }
    private:
      static const int INVALID_ID = -1;
      int iosysid_;
      const std::string name_;
      int iotype_;
      int fh_;
      bool is_in_def_mode_;
      bool del_on_close_;
      std::map<std::string, std::shared_ptr<SPIO_file_obj> > dcache_;

      int create_file(int iosysid, const std::string name, int iotype){
        int ret = PIO_NOERR;

        ret = PIOc_createfile(iosysid, &fh_, &iotype, name.c_str(), PIO_CLOBBER);
        if(ret != PIO_NOERR){
          fh_ = INVALID_ID;
          std::string err_msg("PIOc_createfile(");
          err_msg += "file = " + name
            + ",iotype = " + std::to_string(iotype)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
        }

        return PIO_NOERR;
      }
  };

  class SPIO_dim : public SPIO_valid_dim{
    public:
      SPIO_dim(const std::string &name, PIO_Offset sz):name_(name), id_(INVALID_ID),
        sz_(sz), fh_(INVALID_ID) {}

      std::string name(void ) const override { return name_to_dname(name_); }
      /* To avoid collision with variable names, dim names need to be decorated */
      static std::string name_to_dname(const std::string &name) { return get_dname_prefix() + name; }
      static std::string dname_to_name(const std::string &dname){
        /* FIXME: Use static and move it to cpp */
        const std::string dname_prefix(get_dname_prefix());

        std::string name = dname;
        if(name.find(dname_prefix) == 0){
          name.erase(0, dname_prefix.size());
        }

        return name;
      }
      int id(void ) const override { return id_; }
      PIO_Offset size(void ) const { return sz_; }

      int def(SPIO_file &fh) override {
        int ret = PIO_NOERR;
        
        fh_ = fh.id();
        fname_ = fh.name();
        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Defining dimension : " + name_ +
          " in file " + fname_ + "\n");
        ret = PIOc_def_dim(fh_, name_.c_str(), sz_, &id_);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_def_dim(");
          err_msg += "file = " + fname_
            + "dim name = " + name_
            + ", sz = " + std::to_string(sz_)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        return PIO_NOERR;
      }

      int put(void) override {
        return PIO_NOERR;
      }

      int get_and_verify(void ) const override {
        int ret = PIO_NOERR;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Verifying dimension info : " + name_ +
          " in file " + fname_ + "\n");
        int gid = INVALID_ID;
        ret = PIOc_inq_dimid(fh_, name_.c_str(), &gid);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_inq_dimid(");
          err_msg += "file = " + fname_
            + "dim name = " + name_
            + ", sz = " + std::to_string(sz_)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        if(gid != id_){
          std::string err_msg("Invalid dimension id retrieved from file");
          err_msg += "(file = " + fname_
            + "dim name = " + name_
            + ", sz = " + std::to_string(sz_)
            + ", expected id = " + std::to_string(id_)
            + ", id from file = " + std::to_string(gid)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        char dname[PIO_MAX_NAME + 1];
        PIO_Offset gsz = 0;
        ret = PIOc_inq_dim(fh_, id_, dname, &gsz);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_inq_dim(");
          err_msg += "file = " + fname_
            + "dim name = " + name_
            + ", sz = " + std::to_string(sz_)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        if((std::string(dname) != name_) || (gsz != sz_)){
          std::string err_msg("Invalid dimension name/size retrieved from file");
          err_msg += "(file = " + fname_
            + "dim name = " + name_
            + ", expected sz = " + std::to_string(sz_)
            + ", sz from file = " + std::to_string(gsz)
            + ", expected name = " + name_
            + ", name from file = " + std::string(dname)
            + ")\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        return PIO_NOERR;
      }

    private:
      static const int INVALID_ID = -1;

      const std::string name_;
      int id_;
      PIO_Offset sz_;
      int fh_;
      std::string fname_;

      static inline std::string get_dname_prefix(void ) { return std::string("__SPIO_dim__"); }
  };

  /* FIXME: Use a variant instead */
  class SPIO_att : public SPIO_file_obj{
    public:
      SPIO_att(const std::string &name, const std::string &val) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), vid_(INVALID_ID),
        pio_type_(PIO_CHAR), sz_(val.size()), sval_(val){}
      SPIO_att(const std::string &name, int val) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), vid_(INVALID_ID),
        pio_type_(PIO_INT), sz_(1), ival_(val){}
      SPIO_att(const std::string &name, float val) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), vid_(INVALID_ID),
        pio_type_(PIO_FLOAT), sz_(1), fval_(val){}
      SPIO_att(const std::string &name, double val) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), vid_(INVALID_ID),
        pio_type_(PIO_DOUBLE), sz_(1), dval_(val){}

      std::string name(void ) const override { return name_; }
      int id(void ) const override { return id_; }
      int type(void ) const { return pio_type_; }

      template<typename T>
      T val(void) const{
        switch(pio_type_){
          case PIO_CHAR: return sval_;
          case PIO_INT: return ival_;
          case PIO_FLOAT: return fval_;
          case PIO_DOUBLE : return dval_; 
        }
        throw std::runtime_error("Invalid attribute type, getting attribute val failed");
      }

      int def(SPIO_file &f) override{
        fid_ = f.id();
        fname_ = f.name();
        vid_ = PIO_GLOBAL;
        vname_ = "PIO_GLOBAL";
      
        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Defining global attribute : " + name_ +
          " in file " + fname_ + "\n");
        return put(fid_, vid_);
      }

      int def(SPIO_file &f, SPIO_valid_var &v){
        fid_ = f.id();
        fname_ = f.name();
        vid_ = v.id();
        vname_ = v.name();

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Defining variable attribute : " + name_ +
          " for variable " + vname_ + " in file " + fname_ + "\n");
        return put(fid_, vid_);
      }

      int put(void ) override{
        return PIO_NOERR;
      }

      int get_and_verify(void ) const override{
        int ret = PIO_NOERR;
        assert((fid_ != INVALID_ID) && (vid_ != INVALID_ID));

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Verifying attribute : " + name_ +
          " in file " + fname_ + "\n");
        switch(pio_type_){
          case PIO_CHAR:
            {
              char gval[PIO_MAX_NAME + 1] = "\0";
              ret = PIOc_get_att_text(fid_, vid_, name_.c_str(), gval);
              if(ret == PIO_NOERR){
                if(sval_ != gval){
                  std::string err_msg = "PIOc_get_att_text(file=" + fname_ + ", vname = " + vname_ + ", att_name = " + name_ + ") failed, expected = " + sval_ + ", read = " + std::string(gval) + "\n";
                  Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
                  return PIO_EINTERNAL;
                }
              }
              break;
            }
          case PIO_INT:
            {
              int gval = 0;
              ret = PIOc_get_att(fid_, vid_, name_.c_str(), &gval);
              if(ret == PIO_NOERR){
                if(ival_ != gval){
                  std::string err_msg = "PIOc_get_att(file=" + fname_ + ", vname = " + vname_ + ", att_name = " + name_ + ") failed, expected = " + std::to_string(ival_) + ", read = " + std::to_string(gval) + "\n";
                  Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
                  return PIO_EINTERNAL;
                }
              }
              break;
            }
          case PIO_FLOAT:
            {
              float gval = 0.0;
              ret = PIOc_get_att(fid_, vid_, name_.c_str(), &gval);
              if(ret == PIO_NOERR){
                if(fval_ != gval){
                  std::string err_msg = "PIOc_get_att(file=" + fname_ + ", vname = " + vname_ + ", att_name = " + name_ + ") failed, expected = " + std::to_string(fval_) + ", read = " + std::to_string(gval) + "\n";
                  Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
                  return PIO_EINTERNAL;
                }
              }
              break;
            }
          case PIO_DOUBLE:
            {
              double gval = 0.0;
              ret = PIOc_get_att(fid_, vid_, name_.c_str(), &gval);
              if(ret == PIO_NOERR){
                if(dval_ != gval){
                  std::string err_msg = "PIOc_get_att(file=" + fname_ + ", vname = " + vname_ + ", att_name = " + name_ + ") failed, expected = " + std::to_string(dval_) + ", read = " + std::to_string(gval) + "\n";
                  Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
                  return PIO_EINTERNAL;
                }
              }
              break;
            }
          default:
            throw std::runtime_error("PIOc_get_att() failed, unsupported type");
        }
        if(ret != PIO_NOERR){
          std::string err_msg = "PIOc_get_att() failed : ";
          err_msg += ", fname = " + fname_
                    + ", vname = " + vname_
                    + ", att_name = " + name_
                    + ", pio_type = " + std::to_string(pio_type_) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }
        return PIO_NOERR;
      }

      private:
        static const int INVALID_ID = -2;

        const std::string name_;
        std::string fname_;
        std::string vname_;
        int id_;
        int fid_;
        int vid_;
        int pio_type_;
        int sz_;

        std::string sval_;
        /* Union is probably an overkill here */
        int ival_;
        float fval_;
        double dval_;

        int put(int fh, int vid){
          int ret = PIO_NOERR;
          switch(pio_type_){
            case PIO_CHAR:
              ret = PIOc_put_att_text(fh, vid, name_.c_str(), sz_, static_cast<const char *>(val_ptr())); break;
          default:
            ret = PIOc_put_att(fh, vid, name_.c_str(), pio_type_, sz_, val_ptr());
        }
        if(ret != PIO_NOERR){
          std::string err_msg = "PIOc_put_att() failed : ";
            err_msg += ", fname = " + fname_
                      + ", vname = " + vname_
                      + ". name = " + name_
                      + ", pio_type = " + std::to_string(pio_type_) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        return PIO_NOERR;
      }

      const void *val_ptr(void){
        switch(pio_type_){
          case PIO_CHAR: return static_cast<const void *>(sval_.c_str());
          case PIO_INT: return static_cast<const void *>(&ival_);
          case PIO_FLOAT: return static_cast<const void *>(&fval_);
          case PIO_DOUBLE : return static_cast<const void *>(&dval_); 
          default:
            throw std::runtime_error("Invalid attribute type, getting attribute val failed");
        }
      }
  };

  template<typename T>
  class SPIO_var : public SPIO_valid_var{
    public:
      SPIO_var(const std::string &name,
                const std::vector<std::string> &dims,
                const std::vector<SPIO_att> &atts,
                const std::vector<T> &val) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), dims_(dims), gsz_(0),
        atts_(atts), val_(val) {}

      SPIO_var(const std::string &name,
                const std::vector<std::string> &dims,
                const std::vector<SPIO_att> &atts,
                std::function<T(void)> &val_generator) :
        name_(name), id_(INVALID_ID), fid_(INVALID_ID), dims_(dims), gsz_(0),
        atts_(atts), val_gen_(val_generator) {}

      std::string name(void ) const override { return name_; }
      int id(void ) const override { return id_; }

      int def(SPIO_file &f) override{
        int ret = PIO_NOERR;

        init_file_info(f);

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Defining variable : " + name_ +
          " in file " + fname_ + "\n");
        ret = PIOc_def_var(fid_, name_.c_str(),
                Type_Traits::cxx_to_pio_type<T>::pio_type, dim_ids_.size(),
                dim_ids_.data(), &id_);
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_def_var");
          err_msg += "file = " + fname_
            + ", name = " + name_
            + ", dims = " + Util::String::vec_to_string(dims_)
            + ", dim sizes = " + Util::String::vec_to_string(dim_sz_)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }

        for(std::vector<SPIO_att>::iterator iter = atts_.begin();
              iter != atts_.end(); ++iter){
          ret = iter->def(f, *this);
          if(ret != PIO_NOERR){
            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR,
              "Defining attribute for variable : " + name_ + " failed\n");
            return ret;
          }
        }
        return PIO_NOERR;
      }

      int put(void ) override{
        int ret = PIO_NOERR;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE, "Writing variable : " + name_ +
          " in file " + fname_ + "\n");
        SPIO_var::init_val();

        ret = PIOc_put_var(fid_, id_, val_.data());
        if(ret != PIO_NOERR){
          std::string err_msg("PIOc_put_var");
          err_msg += "file = " + fname_
            + ", name = " + name_
            + ", dims = " + Util::String::vec_to_string(dims_)
            + ", dim sizes = " + Util::String::vec_to_string(dim_sz_)
            + ") failed, ret = " + std::to_string(ret) + "\n";

          Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
          return ret;
        }
        return PIO_NOERR;
      }

      int get_and_verify(void ) const override{
        /*
        int ret = PIO_NOERR;

        switch(Type_Traits::cxx_to_pio_type<T>::pio_type){
          case PIO_CHAR:
          case PIO_INT:
          case PIO_FLOAT:
          case PIO_DOUBLE:
        }
        */

        return PIO_NOERR;
      }

      virtual ~SPIO_var(){}

    protected:
      static const int INVALID_ID = -1;

      const std::string name_;
      std::string fname_;
      int id_;
      int fid_;
      std::vector<std::string> dims_;
      std::vector<int> dim_ids_;
      std::vector<PIO_Offset> dim_sz_;
      std::size_t gsz_;
      std::vector<SPIO_att> atts_;
      std::vector<T> val_;
      std::vector<char> cval_;
      std::vector<T> fval_;
      std::function<T(void)> val_gen_;

      virtual void init_val(void ){
        if(val_gen_){
          assert(val_.size() == 0);
          std::generate_n(std::back_inserter(val_), gsz_, val_gen_);
        }
      }

      void init_file_info(const SPIO_file &f)
      {
        assert(fid_ == INVALID_ID);
        fid_ = f.id();
        fname_ = f.name();

        if(dims_.size() == 0){
          gsz_ = 0;
          return;
        }

        gsz_ = 1;
        std::for_each(dims_.cbegin(), dims_.cend(),
          [&f, this](const std::string &dname){
            std::shared_ptr<const SPIO_dim> pdim = std::dynamic_pointer_cast<const SPIO_dim>(f.get(SPIO_dim::name_to_dname(dname)));
            assert(pdim);

            dim_ids_.push_back(pdim->id());

            PIO_Offset dim_sz = pdim->size();
            dim_sz_.push_back(dim_sz);
            gsz_ *= (dim_sz != PIO_UNLIMITED) ? dim_sz : 1;
          });
      }
  };

  template<>
  inline void SPIO_var<std::string>::init_val(void ){
    if(val_gen_){
      assert(val_.size() == 0);
      std::generate_n(std::back_inserter(val_), gsz_, val_gen_);
    }

    /* Copy strings in val_ to a char vector, cval_ */
    std::for_each(val_.cbegin(), val_.cend(),
      [this](const std::string &str){
        std::copy(str.cbegin(), str.cend(), std::back_inserter(cval_));
      });
  }

  template<typename T>
  class SPIO_cs_var : public SPIO_var<T>{
    public:
      SPIO_cs_var(const std::string &name,
                  const std::vector<std::string> &dims,
                  const std::vector<SPIO_att> &atts,
                  const std::vector<std::vector<PIO_Offset> > &starts,
                  const std::vector<std::vector<PIO_Offset> > &counts,
                  const std::vector<T> &val):
        SPIO_var<T>(name, dims, atts, val), starts_(starts), counts_(counts) {}
      SPIO_cs_var(const std::string &name,
                  const std::vector<std::string> &dims,
                  const std::vector<SPIO_att> &atts,
                  const std::vector<std::vector<PIO_Offset> > &starts,
                  const std::vector<std::vector<PIO_Offset> > &counts,
                std::function<T(void)> &val_generator) :
        SPIO_var<T>(name, dims, atts, val_generator),
          starts_(starts), counts_(counts) {}

      int put(void ) override{
        int ret = PIO_NOERR;
        SPIO_var<T>::init_val();

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE,
          "Writing variable (starts+counts) : " + SPIO_var<T>::name_ +
          " in file " + SPIO_var<T>::fname_ + "\n");
        assert(starts_.size() == counts_.size());
        PIO_Offset val_idx = 0;
        for(std::vector<std::vector<PIO_Offset> >::const_iterator
              citer1 = starts_.cbegin(), citer2 = counts_.cbegin();
              (citer1 != starts_.cend()) && (citer2 != counts_.cend()); ++citer1, ++citer2){

          assert((*citer1).size() == (*citer2).size());
          ret = put(SPIO_var<T>::fid_, SPIO_var<T>::id_, *citer1, *citer2, val_idx);
          if(ret != PIO_NOERR){
            std::string err_msg("PIOc_put_var[a/s]");
            err_msg += "file = " + SPIO_var<T>::fname_
              + ", name = " + SPIO_var<T>::name_
              + ", dims = " + Util::String::vec_to_string(SPIO_var<T>::dims_)
              + ", dim sizes = " + Util::String::vec_to_string(SPIO_var<T>::dim_sz_)
              + ") failed, ret = " + std::to_string(ret) + "\n";

            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
            return ret;
          }

          PIO_Offset nvals = std::accumulate((*citer2).cbegin(), (*citer2).cend(),
                              1, std::multiplies<PIO_Offset>());
          val_idx += nvals;
          if(SPIO_var<T>::cval_.size() == 0){
            if(static_cast<std::size_t>(val_idx) >= SPIO_var<T>::val_.size()) { val_idx = 0; }
          }
          else{
            if(static_cast<std::size_t>(val_idx) >= SPIO_var<T>::cval_.size()) { val_idx = 0; }
          }
        }
        return PIO_NOERR;
      }

    private:
      const std::vector<std::vector<PIO_Offset> > starts_;
      const std::vector<std::vector<PIO_Offset> > counts_;

      int put(int fid, int vid, const std::vector<PIO_Offset> &start,
                      const std::vector<PIO_Offset> &count, PIO_Offset val_idx);
  };

  template<>
  inline int SPIO_cs_var<std::string>::put(int fid, int vid, const std::vector<PIO_Offset> &start,
                                    const std::vector<PIO_Offset> &count,
                                    PIO_Offset val_idx)
  {
    return PIOc_put_vara_text(fid, vid, start.data(), count.data(), &cval_[val_idx]);
  }

  template<>
  inline int SPIO_cs_var<int>::put(int fid, int vid, const std::vector<PIO_Offset> &start,
                            const std::vector<PIO_Offset> &count,
                            PIO_Offset val_idx)
  {
    return PIOc_put_vars_int(fid, vid, start.data(), count.data(), NULL, &val_[val_idx]);
  }

  template<>
  inline int SPIO_cs_var<float>::put(int fid, int vid, const std::vector<PIO_Offset> &start,
                              const std::vector<PIO_Offset> &count,
                              PIO_Offset val_idx)
  {
    return PIOc_put_vars_float(fid, vid, start.data(), count.data(), NULL, &val_[val_idx]);
  }

  template<>
  inline int SPIO_cs_var<double>::put(int fid, int vid, const std::vector<PIO_Offset> &start,
                                const std::vector<PIO_Offset> &count,
                                PIO_Offset val_idx)
  {
      return PIOc_put_vars_double(fid, vid, start.data(), count.data(), NULL, &val_[val_idx]);
  }

  template<typename T>
  class SPIO_unlimited_var : public SPIO_var<T>{
    public:
      SPIO_unlimited_var(const std::string &name,
                          const std::vector<std::string> &dims,
                          const std::vector<SPIO_att> &atts,
                          std::shared_ptr<const SPIO_decomp> pdecomp,
                          int nframes,
                          const std::vector<T> &val):
        SPIO_var<T>(name, dims, atts, val), pdecomp_(pdecomp), nframes_(nframes){}

      SPIO_unlimited_var(const std::string &name,
                          const std::vector<std::string> &dims,
                          const std::vector<SPIO_att> &atts,
                          std::shared_ptr<const SPIO_decomp> &pdecomp,
                          int nframes,
                          std::function<T(void )> &val_generator):
        SPIO_var<T>(name, dims, atts, val_generator), pdecomp_(pdecomp), nframes_(nframes){}

      int put(void ) override{
        int ret = PIO_NOERR;

        Util::GVars::logger->log(Util::Logging::LogLevel::VERBOSE,
          "Writing variable (unlimited dim) : " + SPIO_var<T>::name_ +
          " in file " + SPIO_var<T>::fname_ + "\n");
        SPIO_var<T>::init_val();

        for(int iframe=0; iframe < nframes_; iframe++){
          ret = PIOc_setframe(SPIO_var<T>::fid_, SPIO_var<T>::id_, iframe);
          if(ret != PIO_NOERR){
            std::string err_msg("PIOc_setframe");
            err_msg += "file = " + SPIO_var<T>::fname_
              + ", name = " + SPIO_var<T>::name_
              + ", dims = " + Util::String::vec_to_string(SPIO_var<T>::dims_)
              + ", dim sizes = " + Util::String::vec_to_string(SPIO_var<T>::dim_sz_)
              + ", frame = " + std::to_string(iframe)
              + ") failed, ret = " + std::to_string(ret) + "\n";

            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
            return ret;
          }

          ret = PIOc_write_darray(SPIO_var<T>::fid_, SPIO_var<T>::id_,
                                  pdecomp_->id(), SPIO_var<T>::val_.size(), SPIO_var<T>::val_.data(),
                                  (SPIO_var<T>::fval_.size() != 0) ? SPIO_var<T>::fval_.data() : NULL);
          if(ret != PIO_NOERR){
            std::string err_msg("PIOc_write_darray");
            err_msg += "file = " + SPIO_var<T>::fname_
              + ", name = " + SPIO_var<T>::name_
              + ", dims = " + Util::String::vec_to_string(SPIO_var<T>::dims_)
              + ", dim sizes = " + Util::String::vec_to_string(SPIO_var<T>::dim_sz_)
              + ", frame = " + std::to_string(iframe)
              + ") failed, ret = " + std::to_string(ret) + "\n";

            Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, err_msg);
            return ret;
          }
        }

        return PIO_NOERR;
      }

    private:
      std::shared_ptr<const SPIO_decomp> pdecomp_;
      int nframes_;
  };

} // namespace Util

#endif // __E3SM_FGI_DATA_HPP__
