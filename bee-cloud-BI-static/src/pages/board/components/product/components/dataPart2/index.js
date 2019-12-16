
import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Select, Empty, Radio } from 'antd';
import styles from '../../index.less';
import { Chart, Geom, Axis, Coord, Tooltip, Shape, Legend } from "bizcharts";
import NewRangePicker from "@/components/NewRangePicker";
import { getProductSpec, getProduction } from '../../services/services'
import DataSet from "@antv/data-set";
import { getProductListByCategory, getDeviceListByType } from '@/services/index'

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
      productId: 0,
      furnaceId: 0
    },
    fields1: [],
    fields2: [],
    data1: [],//产量数据
    data2: [],//产品质量数据
    deviceList: [{ name: '全部', id: 0 }],//炉子列表 （按钮组）
    furnaceId: 0,//炉号
    productList: [{ name: '全部', id: 0 }],//产品下拉列表
    loading1: false,//产量
    loading2: false//产品质量
  }

  componentDidMount() {
    this.getDeviceListByType();
    this.getProductListByCategory();
  }

  //获取类型为 成品 的 产品列表
  getDeviceListByType() {
    getDeviceListByType({ types: [0] }).then(productList => {
      this.setState({ deviceList: [...[{ name: '全部', id: 0 }], ...productList] })
    })
  }

  //获取类型为 成品 的 产品列表
  getProductListByCategory() {
    getProductListByCategory(3).then(productList => {
      this.setState({ productList: [...[{ name: '全部', id: 0 }], ...productList] })
    })
  }

  //获取产品质量
  getProduction = (values) => {
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

      getProduction(params).then(res => {
        let data1 = [];
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
          }
        }
        fields1 = fields1.sort((ele1, ele2) => Number(ele1.replaceAll('-', '')) - Number(ele2.replaceAll('-', '')))
        this.setState({
          loading1: false,
          fields1,
          data1
        })
      })
    })
  }

  //获取产量
  getProductSpec = (values) => {
    const { startTime, endTime, productId, timeRange, furnaceId } = this.state.params2;
    this.setState({
      loading2: true
    }, () => {
      let params = {
        startTime,
        endTime,
        productId,
        timeRange,
        furnaceId,
        ...values
      }

      //更新
      this.setState({
        params2: { ...params }
      })

      if (!params.productId) {
        try {
          delete params.productId
        } catch (error) {
        }
      }
      if (!params.furnaceId) {
        try {
          delete params.furnaceId
        } catch (error) {
        }
      }

      getProductSpec(params).then(res => {
        let data2 = [];
        let fields2 = [];
        if (res.code === 1) {

          if (res.object && res.object.furnaces) {
            res.object.furnaces.forEach(item => {
              let newItem = {};
              for (let key in item) {
                if (key !== 'name') {
                  if (!fields2.includes(key + '.')) {
                    fields2.push(key + '.');
                  }
                  newItem[key + '.'] = item[key];
                } else {
                  newItem.name = item[key];
                }
              }
              data2.push(newItem);
            })
          }
        }
        fields2 = fields2.sort((ele1, ele2) => Number(ele1.replaceAll('-', '')) - Number(ele2.replaceAll('-', '')))

        this.setState({
          loading2: false,
          fields2,
          data2
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
      this.getProduction({ timeRange: timeType === 3 ? 3 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
    } else {
      this.getProductSpec({ timeRange: timeType === 3 ? 3 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
    }
  }

  //炉号选择
  onChangePrice(furnaceId) {
    this.getProductSpec({ furnaceId });
  }

  //产品选择
  productChange(productId, type) {
    if (type === 1) {
      this.getProduction({ productId });
    } else {
      this.getProductSpec({ productId });
    }
  }

  render() {
    const { loading1, loading2, data1, data2, productList, params1, deviceList, fields2, fields1, params2 } = this.state;

    return (
      <Row>
        <Col span={12}>
          <Card
            title="生产数据"
            extra={<NewRangePicker showOne={false} checkLength={true} onChange={(params) => this.changeTime(params, 1)} />}
          >
            <Spin spinning={loading1}>
              <p className={styles.col_header}>
                <span>产量</span>
                <Select style={{ width: 120 }} value={params1.productId} onChange={(value) => this.productChange(value, 1)}>
                  {productList && productList.length ? productList.map(item => <Select.Option value={item.id} key={item.id}>{item.name}</Select.Option>) : null}
                </Select>
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
            title="生产数据"
            extra={<NewRangePicker showOne={false} checkLength={true} onChange={(params) => this.changeTime(params, 2)} />}
          >
            <Spin spinning={loading2}>
              <p className={styles.col_header}>
                <span>产出质量</span>
                <div className={styles.group}>
                  {/* <Radio.Group onChange={(e) => this.onChangePrice(e.target.value)} value={params2.furnaceId}>
                    {deviceList && deviceList.length ? deviceList.map(item => <Radio.Button value={item.id} key={item.id}>{item.name}</Radio.Button>) : null}
                  </Radio.Group> */}
                  <div> 炉号：<Select style={{ width: 200, marginRight: 10 }} value={params2.furnaceId} onChange={(value) => this.onChangePrice(value)}>
                    {deviceList && deviceList.length ? deviceList.map(item => <Select.Option value={item.id} key={item.id}>{item.name}</Select.Option>) : null}
                  </Select></div>
                  <div> 产品：<Select style={{ width: 200 }} value={params2.productId} onChange={(value) => this.productChange(value, 2)}>
                    {productList && productList.length ? productList.map(item => <Select.Option value={item.id} key={item.id}>{item.name}</Select.Option>) : null}
                  </Select></div>
                </div>
              </p>
              <div className={styles.wraper}>
                {data2 && data2.length ? <ArcComp data={data2} field={fields2} /> : <Empty />}
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
      <Axis label={{ formatter: (text, item, index) => text + '吨' }} name="数量" />
      <Tooltip
        itemTpl={'<li>' +
          '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
          '<span class="li-name">{name}：</span>' +
          '<span>{value}</span>' +
          '</li>'}
      />
      <Geom type="intervalStack" position="名字*数量" color={"name"} style={{
        stroke: "#fff",
        lineWidth: 1
      }} />
    </Chart>
  )
}