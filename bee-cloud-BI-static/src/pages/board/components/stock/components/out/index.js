import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Radio, message, Empty } from 'antd';
import styles from '../../index.less';
import { G2, Chart, Geom, Axis, Tooltip, Coord, Label, Legend, View, Guide, Shape, Facet, Util } from "bizcharts";
import NewRangePicker from "@/components/NewRangePicker";
import DataSet from "@antv/data-set";
import { getStorage, getOut } from '../../services/services'

export default class Data extends Component {
  state = {
    type: 2,
    startTime: '',
    endTime: '',
    goodsType: 1,
    oneData: [],
    oneFields: [],
    oneLoading: false,
    twoData: [],
    twoFields: [],
    twoLoading: false,
  }

  getBaseOne = (values) => {
    const { type, startTime, endTime, goodsType } = this.state;
    let self = this;
    this.setState({
      oneLoading: true
    }, () => {
      let params = {
        type,
        goodsType,
        startTime,
        endTime,
        ...values
      }
      getOut(params).then(res => {
        if (res.code === 1) {
          let { newArray, fields } = formatData(res.object);
          self.setState({
            oneData: newArray,
            oneFields: fields
          })
        }
        self.setState({
          type: params.type,
          startTime: params.startTime,
          endTime: params.endTime,
          goodsType: params.goodsType,
          oneLoading: false
        })
      })
    })
  }

  getBaseTwo = (values) => {
    const { type, startTime, endTime } = this.state;
    let self = this;
    this.setState({
      twoLoading: true
    }, () => {
      let params = {
        type,
        goodsType: 4,
        startTime,
        endTime,
        ...values
      }
      getOut(params).then(res => {
        if (res.code === 1) {
          let { newArray, fields } = formatData(res.object);
          self.setState({
            twoData: newArray,
            twoFields: fields
          })
        }
        self.setState({
          startTime: params.startTime,
          endTime: params.endTime,
          twoLoading: false
        })
      })
    })
  }

  onChangeOne = (goodsType) => {
    this.getBaseOne({ goodsType });
  }

  changeTime = ({ timeType, dateStrings }) => {
    if (!dateStrings[0] || !dateStrings[1]) {
      return false
    }

    this.getBaseOne({ type: timeType === 3 ? 2 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
    this.getBaseTwo({ type: timeType === 3 ? 2 : 1, startTime: dateStrings[0], endTime: dateStrings[1] });
  }

  render() {
    const { goodsType, oneData, oneFields, oneLoading, twoData, twoFields, twoLoading } = this.state;

    const ds_oneData = new DataSet();
    const dv_oneData = ds_oneData.createView().source(oneData);
    dv_oneData.transform({
      type: "fold",
      fields: oneFields,
      key: "日期",
      value: "出库量" // value字段
    });

    const ds_twoData = new DataSet();
    const dv_twoData = ds_twoData.createView().source(twoData);
    dv_twoData.transform({
      type: "fold",
      fields: twoFields,
      key: "成品日期",
      value: "成品出库量" // value字段
    });

    return (
      <Card
        title="出库情况"
        extra={<NewRangePicker showOne={false} checkLength={true} onChange={this.changeTime} />}
      >
        <Row>
          <Col span={12}>
            <Spin spinning={oneLoading}>
              <div className={styles.col_header}>
                <span>原料出库情况</span>
                <Radio.Group onChange={(e) => this.onChangeOne(e.target.value)} value={goodsType}>
                  <Radio.Button value={1}>主料</Radio.Button>
                  <Radio.Button value={2}>辅料</Radio.Button>
                  <Radio.Button value={3}>其他</Radio.Button>
                </Radio.Group>
              </div>

              {
                oneData.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={dv_oneData} padding={[60, 80, 80, 100]} forceFit>
                    <Legend position="top" offsetY={-6} />
                    <Axis name="日期" />
                    <Axis label={{ formatter: (text, item, index) => text + '吨' }} name="出库量" />
                    <Tooltip
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">{name}：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Geom type="intervalStack" position="日期*出库量" color={"name"} />
                  </Chart>
              }
            </Spin>
          </Col>

          <Col span={12}>
            <Spin spinning={twoLoading}>
              <p className={styles.col_header}>
                <span>成品出库情况</span>
              </p>
              {
                twoData.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={dv_twoData} padding={[60, 80, 80, 100]} forceFit>
                    <Legend position="top" offsetY={-6} />
                    <Axis name="成品日期" />
                    <Axis label={{ formatter: (text, item, index) => text + '吨' }} name="成品出库量" />
                    <Tooltip
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">{name}：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Geom type="intervalStack" position="成品日期*成品出库量" color={"name"} />
                  </Chart>
              }
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}

const formatData = (data) => {
  let newArray = [];
  let fields = [];

  let tempArray = [];
  data.forEach(first => {
    first.data.forEach(second => {
      tempArray.push({
        key: first.key,
        ...second
      })
    })
  })

  //按照bizchart格式化数据
  tempArray.forEach(first => {
    let haveRecord = false;
    if (newArray.length === 0) {
      newArray.push({
        name: first.productName,
        [first.key + '.']: first.productNumber
      })
      fields.push(first.key + '.');
      haveRecord = true
    } else {
      newArray.forEach(second => {
        if (second.name === first.productName) {
          second[first.key + '.'] = first.productNumber;
          haveRecord = true;

          if (!fields.includes(first.key)) {
            fields.push(first.key + '.');
          }
        }
      })
    }

    if (!haveRecord) {
      newArray.push({
        name: first.productName,
        [first.key + '.']: first.productNumber
      })
      if (!fields.includes(first.key)) {
        fields.push(first.key + '.');
      }
    }
  })
  fields = fields.sort((ele1, ele2) => Number(ele1.replaceAll('-', '')) - Number(ele2.replaceAll('-', '')))
  return { newArray, fields };
}