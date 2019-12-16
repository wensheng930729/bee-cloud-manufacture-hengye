
import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Select, Empty } from 'antd';
import styles from '../../index.less';
import { Chart, Geom, Axis, Coord, Guide, Shape } from "bizcharts";
import NewRangePicker from "@/components/NewRangePicker";
import { getRecoveryRate, getPowerFactor } from '../../services/services'
import { getProductListByCategory } from '@/services/index'
import { isArray } from 'lodash'
import { Number } from 'core-js';
const { Html, Arc } = Guide;

// 下面的代码会被作为 cdn script 注入 注释勿删
// CDN START
Shape.registerShape('point', 'pointer', {
  drawShape(cfg, group) {
    let point = cfg.points[0]; // 获取第一个标记点
    point = this.parsePoint(point);
    const center = this.parsePoint({ // 获取极坐标系下画布中心点
      x: 0,
      y: 0,
    });
    // 绘制指针
    group.addShape('line', {
      attrs: {
        x1: center.x,
        y1: center.y,
        x2: point.x,
        y2: point.y,
        stroke: cfg.color,
        lineWidth: 5,
        lineCap: 'round',
      },
    });
    return group.addShape('circle', {
      attrs: {
        x: center.x,
        y: center.y,
        r: 12,
        stroke: cfg.color,
        lineWidth: 4.5,
        fill: '#fff',
      },
    });
  },
});

const cols = {
  value: {
    min: 0,
    max: 100,
    tickInterval: 10,
    nice: true,
  },
};

export default class Polar extends Component {
  state = {
    startTime: '',
    endTime: '',
    recoveryData: [],
    productList: [{ name: '全部', id: 0 }],
    productId: 0,
    recoveryLoading: false,
    amountData: [],
    amountTotal: 0,
    amountLoading: false,
    powerFactor: [],//矿热炉实时功率因素
  }

  componentDidMount() {
    this.getProductListByCategory();
    this.getPowerFactor();
  }

  //获取类型为 成品 的 产品列表
  getProductListByCategory() {
    getProductListByCategory(3).then(productList => {
      this.setState({ productList: [...[{ name: '全部', id: 0 }], ...productList] })
    })
  }

  //查询矿热炉实时功率因素
  getPowerFactor() {
    getPowerFactor().then(res => {
      if (res.code === 1 && isArray(res.object)) {
        this.setState({
          powerFactor: res.object
        })
      }
    })
  }

  //获取回报率
  getRecoveryRate = (values) => {
    const { startTime, endTime, productId } = this.state;
    this.setState({
      recoveryLoading: true
    }, () => {
      let params = {
        startTime,
        endTime,
        productId,
        ...values
      }
      this.setState({
        startTime: params.startTime, endTime: params.endTime,
        productId: params.productId
      })

      if (!params.productId) {
        try {
          delete params.productId
        } catch (error) {
        }
      }

      getRecoveryRate(params).then(res => {
        if (res.code === 1) {
          this.setState({
            recoveryData: res.object
          })
        }
        this.setState({
          recoveryLoading: false
        })
      })
    })
  }

  //获取回报率
  getRecoveryRate = (values) => {
    const { startTime, endTime, productId } = this.state;
    this.setState({
      recoveryLoading: true
    }, () => {
      let params = {
        startTime,
        endTime,
        productId,
        ...values
      }
      this.setState({
        startTime: params.startTime, endTime: params.endTime,
        productId: params.productId
      })

      if (!params.productId) {
        try {
          delete params.productId
        } catch (error) {
        }
      }

      getRecoveryRate(params).then(res => {
        if (res.code === 1) {
          this.setState({
            recoveryData: res.object
          })
        }
        this.setState({
          recoveryLoading: false
        })
      })
    })
  }

  changeTime = ({ timeType, dateStrings }) => {
    this.getRecoveryRate({ startTime: dateStrings[0], endTime: dateStrings[1] });
    // this.getBaseAmount({ type: amountType, startTime: dateStrings[0], endTime: dateStrings[1] });
  }

  productChange(value, type) {
    this.getRecoveryRate({ productId: value });
  }

  render() {
    const { recoveryLoading, amountLoading, recoveryData, productList, productId, powerFactor } = this.state;

    return (
      <Card
        title="生产数据"
        extra={<NewRangePicker onChange={this.changeTime} />}
      >
        <Row>
          <Col span={12}>
            <Spin spinning={recoveryLoading}>
              <p className={styles.col_header}>
                <span>功率因数</span>
              </p>
              <div className={styles.wraper}>
                {powerFactor && powerFactor.length ? powerFactor.map(item => <ArcComp name={item.deviceId} data={[{ value: item.pf * 10 }]} cols={cols} />)
                  : <Empty />}
              </div>
            </Spin>
          </Col>
          <Col span={12}>
            <Spin spinning={amountLoading}>
              <p className={styles.col_header}>
                <span>回收率</span>
                <Select style={{ width: 120 }} value={productId} onChange={(value) => this.productChange(value, 1)}>
                  {productList && productList.length ? productList.map(item => <Select.Option value={item.id} key={item.id}>{item.name}</Select.Option>) : null}
                </Select>
              </p>
              <div className={styles.wraper}>
                {recoveryData && recoveryData.length ? recoveryData.map(item => <ArcComp name={item.furnaceName} data={[{ value: item.recoveryRate / 10 }]} cols={cols} />) : <Empty />}
              </div>
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}

/**
 * 
 * 图表组件
 */
const ArcComp = ({ data, cols, name }) => (
  <Chart height={400} width={360} data={data} scale={cols} padding={[0, 0, 50, 0]}>
    <Coord type="polar" startAngle={-9 / 8 * Math.PI} endAngle={1 / 8 * Math.PI} radius={0.75} />
    <Axis
      name="value"
      zIndex={2}
      line={null}
      label={{
        offset: -16,
        textStyle: {
          fontSize: 18,
          textAlign: 'center',
          textBaseline: 'middle',
        },
      }}
      subTickCount={10}
      subTickLine={{
        length: -8,
        stroke: '#fff',
        strokeOpacity: 1,
      }}
      tickLine={{
        length: -18,
        stroke: '#fff',
        strokeOpacity: 1,
      }}
    />
    <Axis name="1" visible={false} />
    <Guide>
      <Arc
        zIndex={0}
        start={[0, 0.965]}
        end={[100, 0.965]}
        style={{ // 底灰色
          stroke: '#CBCBCB',
          lineWidth: 12,
        }}
      />
      <Arc
        zIndex={1}
        start={[0, 0.965]}
        end={[data[0].value, 0.965]}
        style={{
          stroke: '#1890FF',
          lineWidth: 12,
        }}
      />
      <Html
        position={['50%', '95%']}
        html={() => (`<div style="text-align: center;font-size: 12px!important;"><p style="font-size: 1.75em; color: rgba(0,0,0,0.43);margin: 0;">${name}</p><p style="font-size: 1.75em;color: rgba(0,0,0,0.85);margin: 0;">${((data[0].value || 0) * 10).toFixed(2)}%</p></div>`)}
      />
    </Guide>
    <Geom
      type="point"
      position="value*1"
      shape="pointer"
      color="#1890FF"
      active={false}
      style={{ stroke: '#fff', lineWidth: 1 }}
    />
  </Chart>
)