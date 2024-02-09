
export interface PanelProps<Tid = any, TData = any> {
  onClose: () => void;
  onSave?: () => void;
  id?: Tid
  data?: TData
}
