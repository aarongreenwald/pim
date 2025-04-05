export interface File extends  FileSystemItem<'F'> {
  isPlainText: boolean;
}

export interface Directory extends  FileSystemItem<'D'> {

}

export interface FileSystemItem<T extends FileSystemItemType> {
  type: T;
  path: string;
  fileContent: string | null;
  breadcrumbs: Breadcrumb[];
  directoryContents: DirectoryItem[];
  pendingCommit: boolean;
  diaryInfo: DiaryInfo | null;
}

export interface DirectoryItem {
  openInBrowser: boolean;
  isPlainText: boolean;
  name: string;
  isDirectory: boolean;
  pendingCommit?: boolean;
}

export interface Breadcrumb {
  name: string;
  path: string;
}

export interface DiaryInfo {
  latest: DirectoryItem;
  prev: DirectoryItem | null;
  next: DirectoryItem | null;
}

export type FileSystemItemType = 'F' | 'D';

export interface GitStatus {
  notAdded: string[];
  conflicted: string[];
  created: string[];
  deleted: string[];
  modified: string[];
  renamed: { from: string; to: string; }[];
  staged: string[];
  files: {

    /** Original location of the file, when the file has been moved */
    from?: string

    /** Path of the file */
    path: string;

    /** First digit of the status code of the file, e.g. 'M' = modified.
    Represents the status of the index if no merge conflicts, otherwise represents
    status of one side of the merge. */
    index: string;

    /** Second digit of the status code of the file. Represents status of the working directory
    if no merge conflicts, otherwise represents status of other side of a merge. */
    workingDir: string;
  }[];
  ahead: number;
  behind: number;
  current: string | null;
  tracking: string | null;

  /**
   * Gets whether this represents a clean working branch.
   */
  isClean: boolean;
}

export interface NotesSearchResults {
  names: {
    path: string;
    directory: string;
    fileName: string;
  }[];
  contents: {
    path: string;
    items: {
      lineNumber: number;
      text: string;
    }[];
  }[];
}
