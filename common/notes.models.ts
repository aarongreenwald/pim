export interface NotesPathDto {
    path: string;
    isDirectory: boolean;
    isPlainText: boolean;
    fileContent: string | null;
    breadcrumbs: Breadcrumb[];
    directoryInfo: DirectoryItem[];
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