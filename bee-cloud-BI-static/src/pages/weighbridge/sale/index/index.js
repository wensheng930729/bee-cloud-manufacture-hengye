import { Component } from 'react';
import { Form, Row, Col, Card, Statistic, Divider, message } from 'antd';
import styles from './index.less';
import Will from '../component/will';
import Has from '../component/has';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import * as utils from '@/utils/utils';
import { getWeightMachineWebCount } from '../services/index';

const tabs = [
  {
    key: 'will',
    tab: '待过磅车辆',
  }, {
    key: 'has',
    tab: '已过磅车辆',
  }
]

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    tabkey: 'will',
    counts: {
      alreadyNetWeight: 0, //当前列表已称重净重
      alreadyWeightCar: 0, //当前列表已称重车辆数
      waitWeightCar: 0, //当前列表待称重车辆数
    }
  }

  componentDidMount() {
    this.getCount({ isWeight: 0, type: 2 })
  }

  getCount = (params) => {
    const paramsString = utils.queryString(params)
    getWeightMachineWebCount(paramsString).then(res => {
      if (res && res.code === 1) {
        this.setState({ counts: res.object })
      }
    })
  }

  onTabChange = (key, type) => {
    this.setState({ [type]: key }, () => {
      this.getCount({ isWeight: key === 'will' ? 0 : 1, type: 2 })
    });
  }

  render() {
    const { tabkey, counts } = this.state;
    return (
      <div className={styles.container}>
        {
          tabkey === 'will' ?
            <Row type="flex" justify="center">
              <Col span={4}>
                <Statistic title="当前列表待称重车辆数" value={counts.waitWeightCar} />
              </Col>
            </Row>
            : <Row type="flex" justify="center">
              <Col span={4}>
                <Statistic title="当前列表已称重车辆数" value={counts.alreadyWeightCar} />
              </Col>
              <Col>
                <Divider type="vertical" style={{ height: 61 }} />
              </Col>
              <Col span={4}>
                <Statistic title="当前列表已称重净重" value={counts.alreadyNetWeight} />
              </Col>
            </Row>
        }

        <Card tabList={tabs} activeTabKey={tabkey} onTabChange={key => this.onTabChange(key, 'tabkey')} style={{ marginTop: 24 }}>
          {
            tabkey === 'will' ? <Will /> :
              tabkey === 'has' ? <Has /> : null
          }
        </Card>
      </div>
    )
  }
}