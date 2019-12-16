import { Component } from 'react';
import { Row, Col, Card, Spin, Radio, message, Empty } from 'antd';
import styles from '../../index.less';
import { G2, Chart, Geom, Axis, Tooltip, Coord, Label, Legend, View, Guide, Shape, Facet, Util } from "bizcharts";
import DataSet from "@antv/data-set";
import { getPrice, getAmount, getPay, getRatio, getPosition, getGoods } from '../../services/services'

export default class Position extends Component {
  state = {
    type: 4,
    data: [],
    loading: false,
  }

  componentDidMount() {
    this.getBaseData(4);
  }

  getBaseData = (type) => {
    let self = this;
    this.setState({
      loading: true
    }, () => {
      getPosition(type).then(res => {
        if (res.code === 1) {
          let newArray = [];
          res.object.forEach(item => {
            newArray.push({
              item: item.item,
              "已付款": item.already,
              "预付款": item.expect,
              "应付款": item.should
            })
          })
          self.setState({
            data: newArray
          })
        } else {
        }
        self.setState({
          type,
          loading: false
        })
      })
    })
  }

  onChangeType = (type) => {
    this.getBaseData(type);
  }

  render() {
    const { type, data, loading } = this.state;

    const ds = new DataSet();
    const dv = ds.createView().source(data);
    dv.transform({
      type: "fold",
      fields: ["已付款", "预付款", "应付款"],
      // 展开字段集
      key: "type",
      // key字段
      value: "value" // value字段
    });

    return (
      <Card title="未完成业务账款情况">
        <Row>
          <Col span={24}>
            <Spin spinning={loading}>
              <div className={styles.col_header_2}>
                <Radio.Group onChange={(e) => this.onChangeType(e.target.value)} value={type}>
                  <Radio.Button value={4}>供应商</Radio.Button>
                  <Radio.Button value={1}>主料</Radio.Button>
                  <Radio.Button value={2}>辅料</Radio.Button>
                  <Radio.Button value={3}>成品</Radio.Button>
                </Radio.Group>
              </div>
              {
                data.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={dv} padding={[60, 80, 80, 200]} forceFit>
                    <Legend />
                    <Coord transpose scale={[1, -1]} />
                    <Axis name="item" label={{ offset: 12 }} />
                    <Axis name="value" position="right" />
                    <Tooltip
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">{name}：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Geom type="interval" position="item*value" color={"type"} adjust={[{ type: "dodge", marginRatio: 1 / 32 }]} />
                  </Chart>
              }
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}