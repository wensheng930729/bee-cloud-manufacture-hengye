import { Component } from 'react';
import styles from './index.less';
import Data from './components/data';
import Position from './components/position';
import Goods from './components/goods';
import { getConfig } from '../../services/services';

export default class Sale extends Component {
  state = {
    configs: [],
  };

  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看看板', '查看销售', '', '', '']);
    } catch (error) {}
    getConfig('sale').then(res => {
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
        {configs.includes('30018') ? <Data /> : null}
        {configs.includes('30022') ? <Position /> : null}
        {configs.includes('30023') ? <Goods /> : null}
      </div>
    );
  }
}
