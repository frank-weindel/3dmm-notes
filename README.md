KIDWORLD.CPP
* "This is a class that knows how to create GOKs, Help Balloons and Kidspace script interpreters. It exists so an app can customize default behavior."
* Creates script interpreters
* `GOKD` class = "Base GOK descriptor." ???
    * Inherts `BACO`, so its a "cachable object" (i.e. can go int a CRF, see below)
    * 
* `WOKS` class = "World of Kidspace class."
    * Inherits `GOB`
    * Creates script interpreters
    * !!! !!! !!! Pick up here

GFX.CPP (kauai)
* `GPT` class = "Graphics port"
    * Portable graphics abstraction object containing HDC (for Windows)
    * `PgptNewOffscreen()` static method
        * Create an offscreen port
        * Allocates an in memory bitmap (BITMAPINFO)
* `GNV` class = "Graphics environment"
    * Inherits `BASE` class
    * What the difference between this and GPT???
    * Contains a `GPT` instance
    * Every _FastUpdate is two new `GNV`
    * These are used emphemerally

MOVIE.CPP (engine)
* `MVIE` class = 3MM Movie!
* !!!

Places Script Interpreters are Created (Not including TOOLS)
* (kauai) KIDHELP.CPP -> TXHG::_FRunScript
* (kauai) KIDSPACE.CPP -> GOK::_FSetRep
* (kauai) KIDSPACE.CPP -> GOK::FRunScriptCno
* (studio) STUDIO.CPP -> STDIO::_FOpenStudio
    * "Open studio.chk and start the studio script"
* (studio) UTEST.CPP -> APP::_FInitCrm
    * Read the chunky files specified by _pgstSharedFiles, _pgstBuildingFiles and _pgstStudioFiles and create the global CRM; indices to the Building and Studio CRFs are stored in _pglicrfBuilding and _pglicrfStudio.


KIDSPACE.CPP
* 


GOB.CPP
* `GOB` class = "Graphic Object Class"
    * Seems like the graphics object for the whole screen which is drawn on. !!!
    * But it seems some GOBs are smaller objects within the main GOB
* GOBWIN.CPP = Windows Implementation
* `GOB::FInitScreen()` - "Create the screen gob" 
    * Static member called by APPB.CPP
* `GOB::ShutDown()` - "Shutdown all GOBs"
* `SetPos` - Set position of GOB

BASE.CPP (kauai)
* "Base class. Any instances allocated using NewObj (as opposed to being on the stack) are guaranteed to be zero'ed out. Also provides reference counting and debug lost memory checks."
* Supports "Run-time class determiniation" for many if not all Kauai based classes
    * `RTCLASS_DEC` / `RTCLASS` macros
* `BASE` class = base class as explained above
* `BLL` class = "Base linked list" (inherts base)-

CHUNK.CPP (kauai)
* "Chunky file classes."
    * Very detailed description in CHUNK.CPP!
* `CFL` class = "Chunky FiLe class"
    * Inherits `BLL` (Base Linked List)
    * Serialized chunk forest functions
        * `FWriteChunkTree()` method
            * Combine the indicated chunk and its children into an embedded chunk.
        * `CFL::PcflReadForestFromFlo()` static method
            * Reads a CF from a flo and creates a CFL around the data.
* `kcvnCur` const short = "Chunky Version Number Current"
    * Set to `5`
    * "A file written by this version of chunk.cpp receives this cvn.  Any file with this cvn value has exactly the same file format"
* `kcvnBack` const short = "Chunky Version Number Backwards Compatible"
    * Set to `4`
    * "A file written by this version of chunk.cpp can be read by any version of chunk.cpp whose kcvnCur is >= to this"
* `kcvnMin` const short = "Chunky Version Number Minimum"
    * Set to `1`
    * "A file whose cvn is less than kcvnMin cannot be directly read by this version of chunk.cpp (maybe a converter will read it). (this should be <= kcvnCur)"
* `kcvnMinStnNames` const auto = "Minumum Version for Chunk Strings???"
    * Set to `3`
* `kcvnMinGrfcrp` const auto = "Minimum Version for GRFCRP"
    * Set to `4`
    * Prior to version 4, there were only 4 flags and each one was encoded in a single byte of a long. Version 4 squashed these 4 bytes down into a single byte bitmask, which allows up to 8 flags (for the small index), or 32 (for the large index).
    * This version coincides with "Small Index Support" explained below.
* `kcvnMinSmallIndex` const auto = "Minimum Version for Small Index Support"
    * Set to `4`
    * Prior to version 4, CHUNK.CPP only supported the "Big Version" of the `CRP` struct (fixed header portion of each index entry) (!!! educated guess). 
    * The "Big Version" is still supported, and a conversion is done automatically when the index is read into memory `_FReadIndex()`.
    * This value is NOT referenced anywhere. Likely included for potential future use. ShonK indicates in comments that these values would be used to gradually reduce backwards compatibility.
* `kcvnMinForest` const auto = "Minimum Version for Forest"
* `klwMagicChunky` const long = "Chunky File Signature"
    * `CHN2` big endian / `2NHC` little endian
* `CTG` typedef = "Chunk Tag"
    * `unsigned long`
    * "4 ASCII characters"
    * i.e. "BMDL", "TMAP", "GLCR", "GST " etc.
    * "CFL class does not enforce these conventions, but many Kauai tools assume that CTGs consist of 4 printable characters."
* `CNO` typedef = "Chunk number"
    * `unsigned long`
    * ID of chunk
* `CHID` typedef = "Child ID"
    * `unsigend long`
    * ID of a child chunk
* `CKI` struct = "Chunk Identification"
    * Identifies an arbitrary chunk
    * `CTG` + `CNO`
* `KID` struct = "Child Chunk Identification"
    * Identifies a child within a parent chunk
    * `CKI` + `CHID`
* `ECDF` struct = "Embedded Chunk Descriptor (on file)"
    * I think this has to do with the Forest feature???
* "Acyclic directed graph." !!!
* Parent Child Relationship identified by 5-tuple:
    * ctgParent, cnoParent, ctgChild, cnoChild, chid
* Chunky File High-Level Structure
    * Header
        * `CFP` struct
    * Heap
        * Raw Chunk Data in arbitrary order (heap)
    * Index
        * Index for the chunky file
        * Implemented as `GG` class instance (General Group) `_pggcrp`
            * `GFF` struct describes its on file representation
        * Structure
            * Fixed portion
                * `CRP` typedef
            * List of Children (variable length)
                * List of Children
            * Chunk name (optional) (variable length)
    * Free Map (optional)
        * "Indicates which portions of the heap are currently not being used to store chunk data"
* `grfcrp` common variable = "Chunk Flags???"
    * Single byte bitmask containing a Chunk's Flags
    * See `kcvnMinGrfcrp` for details
* Chunk Flags (unnamed `enum`)
    * `fcrpNil` value
        * Not really a flag, just a 0.
    * `fcrpOnExtra` flag (bit 0)
        * "Data is on the extra file"
        * What does this mean???
    * `fcrpLoner` flag (bit 1)
        * Chunks with the "loner" flag can exist without a parent.
        * If all parents of a chunk without this flag are deleted, that chunk is also deleted.
    * `fcrpPacked` flag (bit 2)
        * "The data is compressed"
    * `fcrpMarkT` flag (bit 3)
        * "Used for consistency checks (see _TValidIndex)"
        * What is this flag???
    * `fcrpForest` flag (bit 4)
        * "The chunk contains a forest of chunks"
        * What does a forest of chunks actually entail???
* `CRP` typedef = "Chunk Index Entry Fixed Portion"
    * Fixed header portion each Chunk Index Entry.
    * Is `CRPBG` or `CRPSM` depending on `CHUNK_BIG_INDEX` precompiler flag.
    * NOTE: The code in `_FReadIndex()` detects which version is present in the file and automatically converts it to the format used by the binary when loading it into memory.
* `CRPOTH` typedef = "Other CRP"
    * Is the struct of the opposite version of `CRP`
    * If `CRP` is the big version, `CRPOTH` is the small one, and vice-versa (determined by `CHUNK_BIG_INDEX` precompiler flag)
* `CRPBG` struct = "CRP (big version)"
    * "Big Version" of CRP
        * This is techincally the "old version" as well. See explanation below.
        * `cb`, `ckid`, `ccrpRef` fields are 32-bit signed integers.
        * Supports a 32-bit signed "run-time id" `rti` field on file
    * `CKI` = Chunk ID
    * `FP` = Location on file
    * `long cb` = Size of data on file
    * `long ckid` = Num child chunks
    * `long ccrpRef` = Num parents
    * `long rti` = "Run-time id"
        * What is this???
    * `ulong grfcrp` = Chunk flags
* `CRPSM` struct = "CRP (small version)"
    * "Small version" of CRP
        * Technically the "new version". See explanation below.
        * Does not support "run-time id" `rti` field
            * It seems this field is handled in a memory structure. See 
    * `CKI` = Chunk ID
    * `FP` = Location on file
    * `ulong luGrfcrpCb` = Size + Chunk Flags
        * (low byte) = Chunk Flags
        * (high 3 bytes) = Size of data on file
    * `ushort ckid` = Num child chunks
    * `ushort ccrpRef` = Num parents
* Why is there both a small version and big version of the `CRP`?
    * Evidence shows that the "Big Version" was part of the original index format. The "Small Version" was introduced in Chunky File Version 4 (see `kcvnMinSmallIndex` const).
    * The intent seems to have been to reduce the size of the index because 32-bit integers for sizes, child/parent counts, etc were likely deemed excessive.
    * The "Big Version" is disabled by default, and the shipped chunky files use the "Small Version" (!!! confirm this).
* `CHUNK_BIG_INDEX` precompiler flag
    * If set, use the `CRPBG` struct (big version) for the `CRP` (index entry fixed portion), otherwise use `CRPSM` struct (small version)
* `RTIE` struct = "Run-time ID Entry"
    * Only exists when `CHUNK_BIG_INDEX` is undefined.
    * `CTG`
    * `CNO`
    * `long rti` = "Run-time ID"
    * !!! PICK-UP HERE
* `CFP` struct = "Chunky File Prefix"
    * Header of Chunky File
    * "Stores signatures, version numbers, and pointers to the index and free map. The heap contains only the raw chunk data. The index is not updated on disk until FSave is called."
* `FSM` struct = "Free Space Map Entry"

FILE.CPP (kauai)
* "File management."
    * Seems to be cross platform file management abstraction for Mac/Windows (FILEWIN.CPP vs FILEMAC.CPP)
* `FIL` class = File class
    * Open/Close/Read/Write/Rename/etc methods for files
* `FP` typedef = "File pointer"
    * `long` expanded
    * Just a pointer to a byte position in a file
* `long cb` common var = "client area size"  / "buffer size"
    * Just a byte size
* `FLO` struct = "File Location Class"
    * Represents position and size of block location within a file
    * Contains
      * `FIL*`
      * `FP`
      * `long cb`
* `FNI` class = "File name class"
* `BLCK` class = "Data Block"
    * A file data block OR memory data block (abstraction)
        * Can only be one at a time
        * Can be `Set()` to point to different files/HQs
        * Supports "windowing" around the underlying data source
    * Contains
        * `bool fPacked` = Compressed???
        * `FLO`
            * This value only applies to file blocks
        * `HQ`
            * This value only applies to memory blocks
        * `long _ibMin` = Block begin offset within HQ
            * This value only applies to memory blocks
            * Can be manipulated with `FMoveMin()`
        * `long _ibLim` = Block end offset within HQ 
            * This value only applies to memory blocks
            * Can be manipulated with `FMoveLim()`
    * Constructors
        * FLO*, bool fPacked /*compressed???*/
        * FIL*, FP, long cb, bool fPacked
        * HQ*, bool fPacked
            * "Assumes ownership of the hq (and sets *phq to hqNil)."
        * void
            * Allocates an unpacked empty block
            * Does not allocate an hq
    * Methods
        * _Many methods for reading/writing from block in various ways_
          * `Rgb` seems to refer to raw byte array pointer
              * What does `Rgb` acronym stand for???
        * `FPacked()`
        * `FPackData()` - Pack the block, if it isn't already
            * If the block is unpacked, pack it. If cfmt is cfmtNil, use the default packing format, otherwise use the one specified. If the block is already packed, this doesn't change the packing format.
        * `FUnpackData()` - Unpack data if it is
        * `CbMem()` - Return the amount of memory the block is using (roughly).
            * If its a file block it returns 0, otherwise the size of the HQ
* `MSNK` class - "Message sink class. Basic interface for output streaming."
    * Basically just an abstract interface for `MSFIL`
* `MSFIL` class - "File based message sink."
    * At first glance this is a utility class that wraps a FIL and has convenient functions for writing lines of text (think logging)
CODEC.CPP (kauai)
* All the compression/decompresion goodness
    * Often referred to as "packed" or "unpacked" in the code. `fPacked` commonly represents this.

UTILINT.CPP (kauai)
* "Scalar, rectangle and point declarations"
* `FPure(f)` macro - 
    * Ensures boolean is pure (always true or false)
    * `f` appears to be hungarian notation for boolean? Or 


UTILMEM.CPP (kauai)
* "Memory handling"
   * Cross platform memory management (MEMMAC.CPP / MEMWIN.CPP)
* `MUTX` class - "Mutex (critical section object)"
    * Wrapper for Win32 Critical Section calls
    * Doesn't seem to support Mac?
* `HQH` struct (MEMWIN.CPP) - HQ header
    * Contains client area size (cb) and lock count (cactLock)
    * Debug mode adds a "lwMagic" long for "detecting memory trashing"
* `HQ` typedef - "Moveable (memory) block"
    * `void *` just a special pointer
    * `FAllocHq()` - allocates a new HQ
    * `FResizePhq()` - resizes existing HQ (may change)
    * `FreePhq()` - frees the HQ
    * `FCopyHq()` - copies HQ into a new HQ
    * `CbOfHq()` - returns client area size of HQ
      * Does this by subtracting HQH size from HQ ptr and returning the `cb` value from the HQH.
    * `PvLockHq()` - Lock HQ
      * returns pointer back to HQ hence 'Pv' hungarian notation)
    * `UnlockHq()` - UnlockHQ

GROUPS.CPP (kauai)
* "Basic Collection Classes"
* The following classes are in the file and inherit eachother as structured
* `GRPB` class = "virtual class supporting all group classes"
    * `GLB` class = "GLB is a virtual class supporting GL and AL"
        * `GL` class = General List
            * "basic dynamic array"
            * GL::PglRead()
                * Static for reading from Block "BLCK"
            * GL::PglNew()
                * Static for creating new GLs
                * ??? What is a cb?
                    * "size of client area"
        * `AL` class = Allocated List
            * "Allocated (fixed index) list class"
    * `GGB` class = "virtual class supporting GG and AG"
        * `GG` class = General Group
            * Chunky File's Index is an instance of this class
        * `AG` class = Allocated Group
    * `GSTB` class = "virtual class supporting GST and AST"
        * `GST` class = General String Table
        * `AST` class = Allocated String Table
* "On File" Structs
    * 3 of the classes have an "on file" version
    * `GLF` struct = "General List on file"
    * `ALF` struct = "Allocated List on File"
    * `GGF` struct = "General Group on File"

APPB.CPP (kauai)
* "Common base application class methods."
* APPBWIN.CPP is the Windows implementation
    * Contains **WinMain** entry point
      * Calls FrameMain() symbol
* APPB is base class for 3DMM's `APP` class
    * 3DMM's `APP` class completely overrides `_FInitOS()`
* `WIG` struct = "Windows Specific Globals"
    * Exists as `vwig` global
    * Values set in `WinMain`, `_FInitOS`
    * Contains things like
        * HINSTANCE
        * command line string
        * HWND (app)
        * HDC (app)
        * HWND ("MDI client window")
        * HACCEL (main accerator table)
        * HWND (next clipboard viewer)
        * main thread ID

UTEST.CPP (studio)
* `APP` class, extends `APPB`
* `KWA` class = "KidWorld for the App Class"
    * Inherits `WOKS` class
    * Something to do with splash screen

CRF.CPP (kauai)
* `CRF` class = "Chunky Resource File"
  * "a cache wrapped around a chunky file."
* `CRM` class = "Chunky Resource Manager"
  * "A list of CRFs"
  * (studio) There is a Global one created by APP::FInitCrm (UTEST.CPP)
* `BACO` class = "Base cacheable object"
  * "an object that can be cached in a CRF."
    * "All cacheable objects must be based on BACO."

