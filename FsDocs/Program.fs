module /// Colored printf
       /// Taken from : https://blogs.msdn.microsoft.com/chrsmith/2008/10/01/f-zen-colored-printf/
       Program

open System
open System.IO
open System.Linq
open MarkdownDeep

[<AutoOpen>]
module Print = 
    let cprintfn c fmt = 
        Printf.kprintf (fun s -> 
            let old = System.Console.ForegroundColor
            try 
                System.Console.ForegroundColor <- c
                System.Console.WriteLine s
            finally
                System.Console.ForegroundColor <- old) fmt
    
    let error (str) = 
        cprintfn ConsoleColor.Red "[Error] : %s" str
        Environment.Exit(-1)

type File = 
    { FileInfo : FileInfo
      Title : string
      UniqueId : Guid }
    
    static member Create(fi : FileInfo) = 
        if fi.Name.Contains(" ") then error <| sprintf "File name cannot contain spaces: %A" fi
        { FileInfo = fi
          Title = fi.Name.Replace("-", " ")
          UniqueId = Guid.NewGuid() }
    
    member this.GetBreadCrumbs(rootDir : DirectoryInfo) = 
        let segments = new ResizeArray<string>()
        let mutable currentDir = this.FileInfo.Directory
        while currentDir.FullName <> rootDir.FullName do
            segments.Add(currentDir.Name)
            currentDir <- currentDir.Parent
        segments.ToArray()
    
    member this.GetUrl(rootDir : DirectoryInfo) = String.Join("/", rootDir |> this.GetBreadCrumbs)

type FileItem = 
    | MarkDownFile of File
    | StaticFile of File
    | LinkFile of File

type DirItem = 
    { DirectoryInfo : DirectoryInfo
      UniqueId : Guid
      Children : ResizeArray<FileSystemItem> }
    static member Create(dirInfo) = 
        { DirectoryInfo = dirInfo
          UniqueId = Guid.NewGuid()
          Children = new ResizeArray<FileSystemItem>() }

and FileSystemItem = 
    | MarkDownFile of File
    | StaticFile of File
    | LinkFile of File
    | Dir of DirItem

type PathItem = 
    | FileInf of FileInfo
    | DirInf of DirectoryInfo

[<AutoOpen>]
module IOHelpers = 
    let (+/) (path1 : string) (path2 : string) = Path.Combine([| path1; path2 |])
    
    let loopDir (dir : string) = 
        if Directory.Exists dir then Directory.EnumerateDirectories(dir)
        else Enumerable.Empty<string>()
    
    let loopFiles (dir : string) = Directory.EnumerateFiles(dir)
    let createDir (dir : string) = Directory.CreateDirectory(dir) |> ignore
    
    let rec emptyDir path = 
        loopFiles path |> Seq.iter File.Delete
        loopDir path |> Seq.iter (fun dirPath -> 
                            emptyDir dirPath
                            Directory.Delete(dirPath, true))
    
    let delDir (path) = 
        let mutable attempt = 0
        
        let rec delete (path) = 
            if attempt <= 3 then 
                try 
                    emptyDir path
                    Directory.Delete(path, true)
                with _ -> 
                    attempt <- attempt + 1
                    delete path
        delete path
    
    let enumerateDirectoryFilesInfo root = 
        let rec traverse (d : DirectoryInfo) = 
            seq { 
                for f in d.GetFiles() do
                    yield FileInf f
                for dd in d.GetDirectories() do
                    yield DirInf dd
                    yield! traverse dd
            }
        traverse (DirectoryInfo(root))

module DirWalker = 
    let generateTree (root : string) = 
        let rootDirInfo = DirectoryInfo(root)
        let rootDirItem = DirItem.Create(rootDirInfo)
        let mutable lastDirItem = rootDirItem
        for item in enumerateDirectoryFilesInfo root do
            match item with
            | FileInf f -> 
                match f.Extension.ToLowerInvariant() with
                | ".md" -> lastDirItem.Children.Add(MarkDownFile <| File.Create(f))
                | _ -> lastDirItem.Children.Add(StaticFile <| File.Create(f))
            | DirInf d -> 
                let d1 = DirItem.Create d
                lastDirItem.Children.Add(Dir d1)
                lastDirItem <- d1
        rootDirItem
    
    /// Generate TOC entry for the given page and folder such that the given page is 
    /// selected and the folder is in open mode.
    /// We generate the below kind of structure:
    /// <ul>
    ///     <li>Entry 1 </li>
    ///     <li>Entry 2 </li>
    /// </ul>
    let generateToc (dirItem : DirItem) (selectedFolder : Guid) (selectedFile : Guid) = 
        let fragments = new ResizeArray<string>()
        
        let rec recurse (d) = 
            for c in d.Children do
                match c with
                | MarkDownFile(f) -> 
                    // Set the selected item CSS element on the list item
                    if f.UniqueId = selectedFile then 
                        fragments.Add(sprintf """<li class="selected">%s</li>""" f.FileInfo.Name)
                    else fragments.Add(sprintf """<li>%s</li>""" f.FileInfo.Name)
                | Dir(d) -> 
                    if d.UniqueId = selectedFolder then fragments.Add("""<ul class="selected">""")
                    else fragments.Add("""<ul>""")
                    /// Function is not tail recursive but that should not be a major 
                    /// issue as we don't expect infinite recursion  
                    recurse d
                    fragments.Add("</ul>")
                | _ -> ()
        recurse dirItem

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
