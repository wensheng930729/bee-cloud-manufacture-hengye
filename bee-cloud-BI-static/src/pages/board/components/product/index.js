import { Component } from 'react';
import DataPart1 from './components/polar';
import DataPart2 from './components/dataPart2';
import Electric from './components/electric';
import Stock from './components/stock';
import Inspection from './components/inspection';
import styles from './index.less';
import { getConfig } from '../../services/services';

export default class Board extends Component {
  state = {
    configs: [],
  };

  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看看板', '查看生产', '', '', '']);
    } catch (error) {}
    getConfig('produce').then(res => {
      if (res.code === 1) {
        let configs = [];
        res.object.forEach(item => {
          configs.push(item.code);
        });
        this.setState({
          configs,
        });
      }
    });
  }

  render() {
    const { configs } = this.state;
    return (
      <div className={styles.container}>
        {configs.includes('30008') ? <DataPart1 /> : null}
        {configs.includes('30012') ? <DataPart2 /> : null}
        {configs.includes('30014') ? <Stock /> : null}
        {configs.includes('30016') ? <Electric /> : null}
        {configs.includes('30017') ? <Inspection /> : null}
      </div>
    );
  }
}
