import {useCallback} from 'react';
import {getDiaryPath} from '../services/server-api';
import {useHistory} from 'react-router-dom';
import {todayAsISODate} from '../common/date.utils';


export const useLoadTodayDiary = () => {
  const history = useHistory()
  return useCallback(() => {
    const today = todayAsISODate()
    getDiaryPath(today, true)
      .then(path => history.push(`/notes?path=${encodeURIComponent(path)}`))
  }, [history])
}
