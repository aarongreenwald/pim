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
}

export interface DirectoryItem {
    openInBrowser: boolean;
    isPlainText: boolean;
    name: string;
    isDirectory: boolean
}

export interface Breadcrumb {
    name: string;
    path: string;
}

export type FileSystemItemType = 'F' | 'D';