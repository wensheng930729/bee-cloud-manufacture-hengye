import { Component } from 'react';
import styles from './index.less';
import Storage from './components/storage';
import Out from './components/out';
import OnTheWay from './components/onTheWay';
import { getConfig } from '../../services/services';

export default class Stock extends Component {
  state = {
    configs: [],
  };

  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看看板', '查看库存', '', '', '']);
    } catch (error) {}
    getConfig('stock').then(res => {
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
        {configs.includes('30024') ? <Storage /> : null}
        {configs.includes('30026') ? <Out /> : null}
        {configs.includes('30028') ? <OnTheWay /> : null}
      </div>
    );
  }
}
