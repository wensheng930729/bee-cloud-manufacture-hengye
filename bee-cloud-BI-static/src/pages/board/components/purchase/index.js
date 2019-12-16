import { Component } from 'react';
import styles from './index.less';
import Data from './components/data';
import Position from './components/position';
import Goods from './components/goods';
import { getConfig } from '../../services/services';
// 代码注释
export default class Purchase extends Component {
  state = {
    configs: [],
  };

  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看看板', '查看采购', '', '', '']);
    } catch (error) {}
    getConfig('purchase').then(res => {
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
        {configs.includes('30002') ? <Data /> : null}
        {configs.includes('30006') ? <Position /> : null}
        {configs.includes('30007') ? <Goods /> : null}
      </div>
    );
  }
}
