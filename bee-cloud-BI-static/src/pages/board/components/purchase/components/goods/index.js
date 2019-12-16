import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Radio, message, Empty } from 'antd';
import styles from '../../index.less';
import { G2, Chart, Geom, Axis, Tooltip, Coord, Label, Legend, View, Guide, Shape, Facet, Util } from "bizcharts";
import NewRangePicker from "@/components/NewRangePicker";
import DataSet from "@antv/data-set";
import { getPrice, getAmount, getPay, getRatio, getPosition, getGoods } from '../../services/services'

export default class Goods extends Component {
  state = {
    startTime: '',
    endTime: '',
    type: 4,
    data: [],
    fields: [],
    loading: false,
  }

  getBaseGoods = (values) => {
    const { startTime, endTime, type } = this.state;
    let self = this;
    this.setState({
      loading: true
    }, () => {
      let params = {
        type,
        startTime,
        endTime,
        ...values
      }
      getGoods(params).then(res => {
        if (res.code === 1) {
          if (res.object.length !== 0) {
            let newArray = [{
              name: '未到数量'
            }, {
              name: '已收数量'
            }];
            let fields = [];
            res.object.forEach(item => {
              newArray[0][item.item] = item.incomplete
              newArray[1][item.item] = item.already;
              fields.push(item.item)
            })
            self.setState({
              data: newArray,
              fields
            })
          } else {
            self.setState({
              data: [],
              fields: []
            })
          }
        }
        self.setState({
          startTime: params.startTime,
          endTime: params.endTime,
          type: params.type,
          loading: false
        })
      })
    })
  }

  onChangeGoods = (type) => {
    this.getBaseGoods({ type });
  }

  changeTime = ({ timeType, dateStrings }) => {
    const { type } = this.state;
    this.getBaseGoods({ type, startTime: dateStrings[0], endTime: dateStrings[1] });
  }

  render() {
    const { type, data, fields, loading } = this.state;
    const ds = new DataSet();
    const dv = ds.createView().source(data);
    dv.transform({
      type: "fold",
      fields: fields,
      // 展开字段集
      key: "名字",
      // key字段
      value: "数量" // value字段
    });

    return (
      <Card
        title="到货情况"
        extra={<NewRangePicker showOne={true} onChange={this.changeTime} />}
      >
        <Row>
          <Col span={24}>
            <Spin spinning={loading}>
              <div className={styles.col_header_2}>
                <Radio.Group onChange={(e) => this.onChangeGoods(e.target.value)} value={type}>
                  <Radio.Button value={4}>供应商</Radio.Button>
                  <Radio.Button value={1}>主料</Radio.Button>
                  <Radio.Button value={2}>辅料</Radio.Button>
                  <Radio.Button value={3}>成品</Radio.Button>
                </Radio.Group>
              </div>
              {
                data.length === 0 ?
                  <Empty /> :
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
                    <Geom type="intervalStack" position="名字*数量" color={"name"} />
                  </Chart>
              }
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}