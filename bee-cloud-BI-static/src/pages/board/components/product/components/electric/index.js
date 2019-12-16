
import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Select, Empty, Radio } from 'antd';
import styles from '../../index.less';
import { Chart, Geom, Axis, Coord, Tooltip, Shape, Legend } from "bizcharts";
import NewRangePicker from "@/components/NewRangePicker";
import { getPowerTonConsume, getPowerConsume } from '../../services/services'
import DataSet from "@antv/data-set";

export default class Polar extends Component {
  state = {
    params1: {//产量筛选参数
      timeRange: 3, //1日，2周，3月，4年
      startTime: '',
      endTime: '',
      productId: 0
    },
    params2: {//产品质量筛选参数
      timeRange: 3, //1日，2周，3月，4年
      startTime: '',
      endTime: '',
    },
    fields1: [],
    fields2: [],
    data1: [],//产量数据
    totalNum1: 0,
    data2: [],//产品质量数据
    loading1: false,//产量
    loading2: false//产品质量
  }

  //获取产品质量
  getPowerConsume = (values) => {
    const { startTime, endTime, productId, timeRange } = this.state.params1;
    this.setState({
      loading1: true
    }, () => {
      let params = {
        startTime,
        endTime,
        productId,
        timeRange,
        ...values
      }
      this.setState({
        params1: { ...params }
      })

      if (!params.productId) {
        try {
          delete params.productId
        } catch (error) {
        }
      }

      getPowerConsume(params).then(res => {
        let data1 = [];
        let totalNum1 = 0;
        let fields1 = [];
        if (res.code === 1) {
          if (res.object && res.object.furnaces) {
            res.object.furnaces.forEach(item => {
              let newItem = {};
              for (let key in item) {
                if (key !== 'name') {
                  if (!fields1.includes(key + '.')) {
                    fields1.push(key + '.');
                  }
                  newItem[key + '.'] = item[key];
                } else {
                  newItem.name = item[key];
                }
              }
              data1.push(newItem);
            })
            totalNum1 = res.object.totalNum1;
          }
        }

        fields1 = fields1.sort((ele1, ele2) => Number(ele1.replaceAll('-', '')) - Number(ele2.replaceAll('-', '')))
        this.setState({
          loading1: false,
          data1,
          totalNum1,
          fields1
        })
      })
    })
  }

  //获取产量
  getPowerTonConsume = (values) => {
    const { startTime, endTime, timeRange } = this.state.params2;
    this.setState({
      loading2: true
    }, () => {
      let params = {
        startTime,
        endTime,
        timeRange,
        ...values
      }
      //更新
      this.setState({
        params2: { ...params }
      })

      getPowerTonConsume(params).then(res => {
        let data2 = [];
        if (res.code === 1) {
          const furnaces = res.object.furnaces && res.object.furnaces.length ? res.object.furnaces[0] : {};
          if (res.object && furnaces) {
            for (var key in furnaces) {
              data2.push({ name: key, value: furnaces[key] });
            }
            this.setState({
              data2
            })
          }
        }
        this.setState({
          data2,
          loading2: false
        })
      })
    })
  }

  //时间选择
  changeTime = ({ timeType, dateStrings }, type) => {
    if (!dateStrings[0] || !dateStrings[1]) {
      return false
    }

    if (type === 1) {
      this.getPowerConsume({ timeRange: timeType === 3 ? 3 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
    } else {
      this.getPowerTonConsume({ timeRange: timeType === 3 ? 3 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
    }
  }

  //产品选择
  productChange(productId, type) {
    if (type === 1) {
      this.getPowerConsume({ productId });
    } else {
      this.getPowerTonConsume({ productId });
    }
  }

  render() {
    const { loading1, loading2, data1, data2, params1, fields1, params2 } = this.state;
    return (
      <Row>
        <Col span={12}>
          <Card
            title="电力消耗"
            extra={<NewRangePicker showOne={false} checkLength={true} onChange={(params) => this.changeTime(params, 1)} />}
          >
            <Spin spinning={loading1}>
              <p className={styles.col_header}>
                <span>消耗</span>
              </p>
              <div className={styles.wraper}>
                {data1 && data1.length ? <ArcComp data={data1} field={fields1} />
                  : <Empty />}
              </div>
            </Spin>
          </Card>
        </Col>
        <Col span={12} style={{ marginLeft: 0 }}>
          <Card
            title="电力消耗"
            extra={<NewRangePicker showOne={false} checkLength={true} onChange={(params) => this.changeTime(params, 2)} />}
          >
            <Spin spinning={loading2}>
              <p className={styles.col_header}>
                <span>吨电耗</span>
              </p>
              <div className={styles.wraper}>
                {data2 && data2.length ? <BarComp data={data2} /> : <Empty />}
              </div>
            </Spin>
          </Card>
        </Col>
      </Row>
    )
  }
}

/**
 * 
 * 图表组件
 */
const ArcComp = ({ data, cols, name, field }) => {
  const ds = new DataSet();
  const dv = ds.createView().source(data).transform({
    type: "fold",
    fields: field,
    // 展开字段集
    key: "名字",
    // key字段
    value: "数量" // value字段
  });
  return (
    <Chart height={400} data={dv} padding={[60, 80, 80, 100]} forceFit>
      <Legend position="top" />
      <Axis name="名字" />
      <Axis name="数量" label={{ formatter: (text, item, index) => text + '度' }} />
      <Tooltip
        itemTpl={'<li>' +
          '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
          '<span class="li-name">{name}：</span>' +
          '<span>{value}</span>' +
          '</li>'}
      />
      <Geom type="intervalStack" position="名字*数量" color={"name"} />
    </Chart>
  )
}

/**
 * 
 * 图表组件
 */
const BarComp = ({ data, cols, name, field }) => {
  return (
    <Chart height={400} data={data} scale={{
      sales: {
        tickInterval: 20
      }
    }} padding={[60, 80, 80, 100]} forceFit>
      <Axis name="name" />
      <Axis name="value" label={{ formatter: (text, item, index) => text + '度' }} />
      <Tooltip
        crosshairs={{ type: "cross" }}
        itemTpl={'<li>' +
          '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
          '<span class="li-name">吨电耗：</span>' +
          '<span>{value}</span>' +
          '</li>'}
      />
      <Geom type="interval" position="name*value" />
    </Chart>
  )
}
