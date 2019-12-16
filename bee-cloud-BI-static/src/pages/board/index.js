import { Component } from 'react';
import { Row, Col, Card, Statistic, Tooltip, message } from 'antd';
import styles from './index.less';
import Purchase from './components/purchase';
import Product from './components/product';
import Sale from './components/sale';
import Stock from './components/stock';
import { getBoard, getConfig } from './services/services';

const tabs = [
  {
    key: 'purchase',
    tab: '采购',
  }, {
    key: 'product',
    tab: '生产',
  }, {
    key: 'sale',
    tab: '销售',
  }, {
    key: 'stock',
    tab: '库存',
  }
]

export default class Board extends Component {
  state = {
    canView: false,
    tabkey: 'purchase',
    statistics: [
      {
        title: '今日销售额',
        value: 0,
        yearValue: 0,
        suffix: '元',
        content: '年度总销售额'
      }, {
        title: '今日采购额',
        value: 0,
        yearValue: 0,
        suffix: '元',
        content: '年度总采购额'
      }, {
        title: '今日生产量',
        value: 0,
        yearValue: 0,
        suffix: '吨',
        content: '年度总生产量'
      }
    ]
  }

  componentDidMount() {
    getConfig('data_overview').then(res => {
      if (res.code === 1 && res.object.length !== 0) {
        this.setState({
          canView: true
        })
      }
    })
    getBoard().then(res => {
      if (res.code === 1) {
        this.setState({
          statistics: [
            {
              title: '今日销售额',
              value: res.object.saleDay || 0,
              yearValue: res.object.saleYear || 0,
              suffix: '元',
              content: '年度总销售额'
            }, {
              title: '今日采购额',
              value: res.object.buyDay || 0,
              yearValue: res.object.buyYear || 0,
              suffix: '元',
              content: '年度总采购额'
            }, {
              title: '今日生产量',
              value: res.object.proDay || 0,
              yearValue: res.object.proYear || 0,
              suffix: '吨',
              content: '年度总生产量'
            }
          ]
        })
      } else {
        this.setState({
          statistics: [
            {
              title: '今日销售额',
              value: 0,
              yearValue: 0,
              suffix: '元',
              content: '年度总销售额'
            }, {
              title: '今日采购额',
              value: 0,
              yearValue: 0,
              suffix: '元',
              content: '年度总采购额'
            }, {
              title: '今日生产量',
              value: 0,
              yearValue: 0,
              suffix: '吨',
              content: '年度总生产量'
            }
          ]
        })
      }
    })
  }

  onTabChange = (key, type) => {
    this.setState({ [type]: key });
  }

  render() {
    const { canView, tabkey, statistics } = this.state;
    return (
      <div className={styles.container}>
        {
          canView && (
            <Row style={{ backgroundColor: "#FFFFFF" }}>
              {
                statistics.map((item, index) => (
                  <Col className={styles.statistic_col} key={index}>
                    <Card>
                      <Statistic title={item.title} value={item.value || 0} suffix={item.suffix} />
                      <Tooltip placement="bottom" title={`${item.yearValue || 0}${item.suffix}`}>
                        <p style={{ margin: "12px 0" }}>{item.content}</p>
                      </Tooltip>
                    </Card>
                  </Col>
                ))
              }
            </Row>
          )
        }
        <Card tabList={tabs} activeTabKey={tabkey} onTabChange={key => this.onTabChange(key, 'tabkey')} style={{ marginTop: 24 }}>
          {
            tabkey === 'purchase' ? <Purchase /> :
              tabkey === 'product' ? <Product /> :
                tabkey === 'sale' ? <Sale /> :
                  tabkey === 'stock' ? <Stock />
                    : null
          }
        </Card>
      </div >
    )
  }
} 